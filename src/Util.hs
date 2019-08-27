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

import ADL.Config(ToolConfig(..), DeployMode(..), JsonSource(..), ProxyModeConfig(..))
import ADL.Release(ReleaseConfig(..))
import ADL.Core(adlFromJsonFile', runJsonParser, textFromParseContext, AdlValue(..), ParseResult(..))
import ADL.Types(StaticConfigName)
import Blobs(releaseBlobStore, BlobStore(..), BlobName)
import Blobs(releaseBlobStore, BlobStore(..), BlobName)
import Codec.Archive.Zip(withArchive, unpackInto)
import Control.Concurrent(threadDelay)
import Control.Monad(when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List(isPrefixOf,sortOn)
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
import Util.Aws(mkAwsEnvFn)
import Path(Path,Abs,Dir,File,parseAbsDir,parseAbsFile)
import Types(IOR, REnv(..), getToolConfig, scopeInfo, info)

configContextCacheFilePath :: ToolConfig -> StaticConfigName -> FilePath
configContextCacheFilePath tcfg name = T.unpack (tc_contextCache tcfg) </> T.unpack name <> ".json"

--- Download the infrastructure context files from the blobstore
fetchConfigContext :: Maybe Int -> IOR ()
fetchConfigContext retryAfter = do
  scopeInfo "Fetching deploy context from store" $ do
    tcfg <- getToolConfig
    awsEnvFn <- mkAwsEnvFn
    do
      liftIO $ createDirectoryIfMissing True (T.unpack (tc_contextCache tcfg))

      for_ (M.toList (SM.toMap (tc_configSources tcfg))) $ \name_src -> do
        let name = fst name_src
        let src = snd name_src
        let cacheFilePath = configContextCacheFilePath tcfg name
        case retryAfter of
          Nothing -> return ()
          Just delay -> await awsEnvFn src delay

        info ("Downloading " <> T.pack cacheFilePath <> " from " <> jsrcLabel src)
        jsrcFetchToFile awsEnvFn src cacheFilePath
 where
   await :: IOR AWS.Env -> JsonSource -> Int -> IOR ()
   await awsEnvFn jsrc delaySecs = do
     exists <- jsrcExists awsEnvFn jsrc
     case exists of
       True -> return ()
       False -> do
         info ("Waiting for "<> jsrcLabel jsrc)
         liftIO $ threadDelay (1000000 * delaySecs)
         await awsEnvFn jsrc delaySecs

   jsrcLabel :: JsonSource -> T.Text
   jsrcLabel (Jsrc_file file) = file
   jsrcLabel (Jsrc_s3 s3Path) = s3Path
   jsrcLabel (Jsrc_awsSecretArn arn) = "AWS secret " <> arn

   jsrcExists :: IOR AWS.Env -> JsonSource -> IOR Bool
   jsrcExists _ (Jsrc_file file) = do
     liftIO $ doesFileExist (T.unpack file)
   jsrcExists awsEnvFn (Jsrc_s3 s3Path) = do
     env <- awsEnvFn
     let (bucket,key)  = S3.splitPath s3Path
     liftIO $ S3.fileExists env bucket key
   jsrcExists awsEnvFn (Jsrc_awsSecretArn arn) = do
     env <- awsEnvFn
     liftIO $ Secrets.secretExists env arn

   jsrcFetchToFile :: IOR AWS.Env -> JsonSource -> FilePath -> IOR ()
   jsrcFetchToFile _ (Jsrc_file fromFile) toFile = do
     liftIO $ copyFile (T.unpack fromFile) toFile
   jsrcFetchToFile awsEnvFn (Jsrc_s3 s3Path) toFile =  do
     env <- awsEnvFn
     let (bucket,key)  = S3.splitPath s3Path
     liftIO $ S3.downloadFileFrom env bucket key toFile Nothing
   jsrcFetchToFile awsEnvFn (Jsrc_awsSecretArn arn) toFile = do
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

  values <- for (M.toList (SM.toMap (tc_configSources tcfg))) $ \name_src -> do
    let name = fst name_src
    let src = snd name_src
    let cacheFilePath = configContextCacheFilePath tcfg name
    lbs <- LBS.readFile cacheFilePath
    case JS.eitherDecode' lbs of
     (Left e) -> error ("Unable to parse json from " <> cacheFilePath)
     (Right jv) -> return (takeBaseName cacheFilePath, jv)

  return (JS.Object (HM.fromList ([(T.pack file,jv) | (file,jv) <- values]) ))

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

adlFromYamlByteString :: forall a . (AdlValue a) => LBS.ByteString -> (ParseResult a)
adlFromYamlByteString lbs = case Y.decodeEither' (LBS.toStrict lbs) of
  (Left e) -> ParseFailure ("Invalid yaml:" <> T.pack (Y.prettyPrintParseException e)) []
  (Right jv) -> runJsonParser jsonParser [] jv

decodeAdlParseResult :: forall a . (AdlValue a) => T.Text -> ParseResult a -> Either T.Text a
decodeAdlParseResult from (ParseFailure e ctx) = Left
  (  "Unable to parse a value of ADL type "
  <> atype (Proxy :: Proxy a)
  <> from <> ": "
  <> e <> " at " <> textFromParseContext ctx
  )
decodeAdlParseResult _ (ParseSuccess a) = Right a

readFileOrS3 :: IO AWS.Env -> FilePath -> IO (Maybe LBS.ByteString)
readFileOrS3 getAwsEnv configPath | isPrefixOf "s3://" configPath = do
  let (s3bucket,s3path) = S3.splitPath (T.pack configPath)
  awsEnv <- getAwsEnv
  exists <- S3.fileExists awsEnv s3bucket s3path
  if exists
    then Just <$> S3.readFile awsEnv s3bucket s3path
    else return Nothing
readFileOrS3 _ configPath = do
  exists <- doesFileExist configPath
  if exists
    then Just <$> LBS.readFile configPath
    else return Nothing
