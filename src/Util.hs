{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Util where
  
import qualified Data.Map as M
import qualified ADL.Core.StringMap as SM
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import qualified Log as L
import qualified Text.Mustache as TM
import qualified Text.Mustache.Types as TM
import qualified Blobs.S3 as S3
import qualified Blobs.Secrets as Secrets

import qualified Control.Monad.Trans.AWS as AWS

import ADL.Config(ToolConfig(..), DeployMode(..), DeployContext(..), DeployContextSource(..), ProxyModeConfig(..))
import ADL.Release(ReleaseConfig(..))
import ADL.Core(adlFromJsonFile', runJsonParser, textFromParseContext, AdlValue(..), ParseResult(..))
import ADL.Types(DeployConfigTopicName)
import Blobs(releaseBlobStore, BlobStore(..), BlobName)
import Blobs(releaseBlobStore, BlobStore(..), BlobName)
import Codec.Archive.Zip(withArchive, unpackInto)
import Control.Concurrent(threadDelay)
import Control.Monad(when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List(sortOn)
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.Proxy
import Data.Foldable(for_)
import Data.Time.Clock.POSIX(getCurrentTime)
import Data.Traversable(for)
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, (</>))
import System.Posix.Files(createSymbolicLink, removeLink)
import System.IO(stdout, withFile, hIsEOF, IOMode(..))
import System.Directory(listDirectory, doesFileExist, copyFile)
import Path(Path,Abs,Dir,File,parseAbsDir,parseAbsFile)
import Types(IOR, REnv(..), getToolConfig, scopeInfo, info)

deployContextCacheFilePath :: ToolConfig -> DeployContext -> FilePath
deployContextCacheFilePath tcfg dc = T.unpack (tc_contextCache tcfg) </> T.unpack (dc_name dc) <> ".json"

configContextCacheFilePath :: ToolConfig -> DeployConfigTopicName -> FilePath
configContextCacheFilePath tcfg dctn = T.unpack (tc_contextCache tcfg) </> T.unpack dctn <> ".json"

--- Download the infrastructure context files from the blobstore
fetchDeployContext :: Maybe Int -> IOR ()
fetchDeployContext retryAfter = do
  scopeInfo "Fetching deploy context from store" $ do
    tcfg <- getToolConfig
    awsEnvFn <- S3.mkAwsEnvFn
    do
      liftIO $ createDirectoryIfMissing True (T.unpack (tc_contextCache tcfg))

      for_ (M.toList (SM.toMap (tc_configContexts tcfg))) $ \dctn_dcs -> do
        let dctn = fst dctn_dcs
        let dcs = snd dctn_dcs
        let cacheFilePath = configContextCacheFilePath tcfg dctn
        case retryAfter of
          Nothing -> return ()
          Just delay -> await awsEnvFn dcs delay

        info ("Downloading " <> T.pack cacheFilePath <> " from " <> dcLabel dcs)
        dcFetchToFile awsEnvFn dcs cacheFilePath

      for_ (tc_deployContexts tcfg) $ \dc -> do
        let cacheFilePath = deployContextCacheFilePath tcfg dc
        case retryAfter of
          Nothing -> return ()
          Just delay -> await awsEnvFn (dc_source dc) delay

        info ("Downloading " <> T.pack cacheFilePath <> " from " <> dcLabel (dc_source dc))
        dcFetchToFile awsEnvFn (dc_source dc) cacheFilePath
 where
   await :: IOR AWS.Env -> DeployContextSource -> Int -> IOR ()
   await awsEnvFn dc delaySecs = do
     exists <- dcExists awsEnvFn dc
     case exists of
       True -> return ()
       False -> do
         info ("Waiting for "<> dcLabel dc)
         liftIO $ threadDelay (1000000 * delaySecs)
         await awsEnvFn dc delaySecs

   dcLabel :: DeployContextSource -> T.Text
   dcLabel (Dcs_file file) = file
   dcLabel (Dcs_s3 s3Path) = s3Path
   dcLabel (Dcs_awsSecretArn arn) = "AWS secret " <> arn

   dcExists :: IOR AWS.Env -> DeployContextSource -> IOR Bool
   dcExists _ (Dcs_file file) = do
     liftIO $ doesFileExist (T.unpack file)
   dcExists awsEnvFn (Dcs_s3 s3Path) = do
     env <- awsEnvFn
     let (bucket,key)  = S3.splitPath s3Path
     liftIO $ S3.fileExists env bucket key
   dcExists awsEnvFn (Dcs_awsSecretArn arn) = do
     env <- awsEnvFn
     liftIO $ Secrets.secretExists env arn

   dcFetchToFile :: IOR AWS.Env -> DeployContextSource -> FilePath -> IOR ()
   dcFetchToFile _ (Dcs_file fromFile) toFile = do
     liftIO $ copyFile (T.unpack fromFile) toFile
   dcFetchToFile awsEnvFn (Dcs_s3 s3Path) toFile =  do
     env <- awsEnvFn
     let (bucket,key)  = S3.splitPath s3Path
     liftIO $ S3.downloadFileFrom env bucket key toFile Nothing
   dcFetchToFile awsEnvFn (Dcs_awsSecretArn arn) toFile = do
     env <- awsEnvFn
     liftIO $ Secrets.downloadSecretFrom env arn toFile

injectContext :: (JS.Value -> JS.Value) -> FilePath -> FilePath -> IOR()
injectContext modifyContextFn templatePath destPath = do
  tcfg <- getToolConfig
  liftIO $ do
    -- load and merge the infrastructure context
    ctx <- fmap modifyContextFn (loadMergedContext tcfg)

    -- interpolate the context into each templated file
    expandTemplateFileToDest ctx templatePath destPath

-- unpack a release into the specified directory, and expand any templates
--
-- `modifyContextFn` can be used to modify the context before it is used to
-- expand templates
unpackRelease :: (JS.Value -> JS.Value) -> T.Text -> FilePath -> IOR ()
unpackRelease modifyContextFn release toDir = do
  scopeInfo ("Unpacking release " <> " to " <> T.pack toDir) $ do
    tcfg <- getToolConfig
    bs <- releaseBlobStore
    liftIO $ do
      let releaseFilePath = toDir </> T.unpack release
      createDirectoryIfMissing True toDir

      -- download the release zip file
      bs_fetchToFile bs release releaseFilePath

      -- expand the zip file
      withArchive releaseFilePath (unpackInto toDir)

      -- load the release metadata
      rcfg <- adlFromJsonFile' (toDir </> "release.json")

      -- load and merge the infrastructure context
      ctx <- fmap modifyContextFn (loadMergedContext tcfg)

      -- interpolate the context into each templated file
      for_ (rc_templates rcfg) $ \templatePath -> do
        expandTemplateFile ctx (toDir </> T.unpack templatePath)

checkReleaseExists :: T.Text -> IOR ()
checkReleaseExists release = do
  bs <- releaseBlobStore
  exists <- liftIO $ bs_exists bs release
  when (not exists) $ do
    error ("Release " <> T.unpack release <> " does not exist in S3")
  return ()

loadMergedContext :: ToolConfig -> IO JS.Value
loadMergedContext tcfg = do
  let cacheDir = T.unpack (tc_contextCache tcfg)

  valuesx <- for (M.toList (SM.toMap (tc_configContexts tcfg))) $ \dctn_dcs -> do
    let dctn = fst dctn_dcs
    let dcs = snd dctn_dcs
    let cacheFilePath = configContextCacheFilePath tcfg dctn
    lbs <- LBS.readFile cacheFilePath
    case JS.eitherDecode' lbs of
     (Left e) -> error ("Unable to parse json from " <> cacheFilePath)
     (Right jv) -> return (takeBaseName cacheFilePath, jv)
  
  values <- for (tc_deployContexts tcfg) $ \dc -> do
    let cacheFilePath = deployContextCacheFilePath tcfg dc
    lbs <- LBS.readFile cacheFilePath
    case JS.eitherDecode' lbs of
     (Left e) -> error ("Unable to parse json from " <> cacheFilePath)
     (Right jv) -> return (takeBaseName cacheFilePath, jv)
  return (JS.Object (HM.fromList ([(T.pack file,jv) | (file,jv) <- values] ++ [(T.pack file,jv) | (file,jv) <- valuesx ]) ))

expandTemplateFileToDest :: JS.Value -> FilePath -> FilePath -> IO ()
expandTemplateFileToDest ctx templatePath destPath = do
  etemplate <- TM.automaticCompile [takeDirectory templatePath] templatePath
  case etemplate of
   Left err -> error (show err)
   Right template -> do
     let text = TM.substitute template ctx
         outfile = destPath
     T.writeFile outfile text

expandTemplateFile :: JS.Value -> FilePath -> IO ()
expandTemplateFile ctx templatePath = do
  expandTemplateFileToDest ctx templatePath (dropExtension templatePath)

toDirPath :: FilePath -> Path Abs Dir
toDirPath path = case parseAbsDir path of
  Just p -> p
  Nothing -> error "Unable to parse directory"

toFilePath :: FilePath -> Path Abs File
toFilePath path = case parseAbsFile path of
  Just p -> p
  Nothing -> error "Unable to parse directory"

removeNullKeys :: JS.Value -> JS.Value
removeNullKeys (JS.Object hm) = (JS.Object (fmap removeNullKeys (HM.filter (not . isNull) hm)))
  where
    isNull JS.Null = True
    isNull _ = False
removeNullKeys (JS.Array a) = (JS.Array (fmap removeNullKeys a))
removeNullKeys json = json

adlFromYamlFile' :: forall a . (AdlValue a) => FilePath -> IO a
adlFromYamlFile' file = do
  ejv <- Y.decodeFileEither file
  case ejv of
    (Left e) -> ioError $ userError (Y.prettyPrintParseException e)
    (Right jv) ->
      case runJsonParser jsonParser [] jv of
        (ParseFailure e ctx) -> ioError $ userError $
          T.unpack
            (  "Unable to parse a value of ADL type "
            <> atype (Proxy :: Proxy a)
            <> " from " <>  T.pack file <> ": "
            <> e <> " at " <> textFromParseContext ctx
            )
        (ParseSuccess a) -> return a
