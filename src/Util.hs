{-# LANGUAGE OverloadedStrings #-}
module Util where

import qualified ADL.Core.StringMap as SM
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Log as L
import qualified Network.AWS.ECR as ECR
import qualified Network.AWS.S3 as S3
import qualified Text.Mustache as TM
import qualified Text.Mustache.Types as TM

import ADL.Config(ToolConfig(..), DeployContextFile(..), DeployMode(..), ProxyModeConfig(..))
import ADL.Release(ReleaseConfig(..))
import ADL.Core(adlFromJsonFile')
import Codec.Archive.Zip(withArchive, unpackInto)
import Control.Concurrent(threadDelay)
import Control.Exception.Lens
import Control.Monad(when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Lens
import Data.List(sortOn)
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.Conduit((.|), ($$), ($$+-))
import Data.Conduit.Binary(sinkFile)
import Data.Foldable(for_)
import Data.Time.Clock.POSIX(getCurrentTime)
import Data.Traversable(for)
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, (</>))
import System.Posix.Files(createSymbolicLink, removeLink)
import System.IO(stdout, withFile, hIsEOF, IOMode(..))
import System.Process(callProcess,callCommand)
import Network.AWS.Data.Body(RsBody(..))
import Network.AWS.Data.Text(ToText(..))
import Network.HTTP.Types.Status(notFound404)
import Path(Path,Abs,Dir,File,parseAbsDir,parseAbsFile)
import Types(IOR, REnv(..), getToolConfig, scopeInfo)

mkAwsEnv :: IOR Env
mkAwsEnv = do
  logger <- fmap re_logger ask
  liftIO $ do
    env0 <- newEnv Discover
    return (env0 & envLogger .~ (L.awsLogger logger))

splitS3Path :: T.Text -> (S3.BucketName,S3.ObjectKey)
splitS3Path s3Path = case T.stripPrefix "s3://" s3Path of
  Nothing -> error "s3Path must start with s3://"
  (Just s) ->
    case T.breakOn "/" s of
      (_,"") -> error "s3Path must include a key"
      (b,k) -> (S3.BucketName b,S3.ObjectKey (T.tail k))

--- Download the infrastructure context files from S3
fetchDeployContext :: Maybe Int -> IOR ()
fetchDeployContext retryAfter = do
  scopeInfo "Fetching deploy context from AWS" $ do
    tcfg <- getToolConfig
    env <- mkAwsEnv
    liftIO $ do
      let cacheDir = T.unpack (tc_contextCache tcfg)
      createDirectoryIfMissing True cacheDir
      for_ (tc_deployContextFiles tcfg) $ \cf -> do
        let cacheFilePath = cacheDir </> T.unpack (dcf_name cf)
            (bucketName,objectKey) = splitS3Path (dcf_source cf)
        downloadFileFromS3 env bucketName objectKey cacheFilePath retryAfter

-- unpack a release into the specified directory, and expand any templates
--
-- `modifyContextFn` can be used to modify the context before it is used to
-- expand templates
unpackRelease :: (JS.Value -> JS.Value) -> T.Text -> FilePath -> IOR ()
unpackRelease modifyContextFn release toDir = do
  scopeInfo ("Unpacking release " <> " to " <> T.pack toDir) $ do
    tcfg <- getToolConfig
    env <- mkAwsEnv
    liftIO $ do
      createDirectoryIfMissing True toDir
      let (bucketName,S3.ObjectKey bucketPath) = splitS3Path (tc_releasesS3 tcfg)
          objectKey = S3.ObjectKey (bucketPath <> "/" <> release)
          releaseFilePath = toDir </> T.unpack release

      -- download the release zip file
      downloadFileFromS3 env bucketName objectKey releaseFilePath Nothing

      -- expand the zip file
      withArchive (toFilePath releaseFilePath) (unpackInto (toDirPath toDir))

      -- load the release metadata
      rcfg <- adlFromJsonFile' (toDir </> "release.json")

      -- load and merge the infrastructure context
      ctx <- fmap modifyContextFn (loadMergedContext tcfg)

      -- interpolate the context into each templated file
      for_ (rc_templates rcfg) $ \templatePath -> do
        expandTemplateFile ctx (toDir </> T.unpack templatePath)

unpackRelease' :: T.Text -> FilePath -> IOR ()
unpackRelease' = unpackRelease id

loadMergedContext :: ToolConfig -> IO JS.Value
loadMergedContext tcfg = do
  let cacheDir = T.unpack (tc_contextCache tcfg)
  values <- for (tc_deployContextFiles tcfg) $ \cf -> do
    let cacheFilePath = cacheDir </> T.unpack (dcf_name cf)
    lbs <- LBS.readFile cacheFilePath
    case JS.eitherDecode' lbs of
     (Left e) -> error ("Unable to parse json from " <> cacheFilePath)
     (Right jv) -> return (takeBaseName cacheFilePath, jv)
  return (JS.Object (HM.fromList [(T.pack file,jv) | (file,jv) <- values]))

expandTemplateFile :: JS.Value -> FilePath -> IO ()
expandTemplateFile ctx templatePath = do
  etemplate <- TM.automaticCompile [takeDirectory templatePath] templatePath
  case etemplate of
   Left err -> error (show err)
   Right template -> do
     let text = TM.substitute template ctx
         outfile = dropExtension templatePath
     T.writeFile outfile text

toDirPath :: FilePath -> Path Abs Dir
toDirPath path = case parseAbsDir path of
  Just p -> p
  Nothing -> error "Unable to parse directory"

toFilePath :: FilePath -> Path Abs File
toFilePath path = case parseAbsFile path of
  Just p -> p
  Nothing -> error "Unable to parse directory"


downloadFileFromS3 :: Env -> S3.BucketName -> S3.ObjectKey -> FilePath -> Maybe Int -> IO ()
downloadFileFromS3 env bucketName  objectKey toFilePath retryAfter = do
  handling _ServiceError onServiceError $ do
    T.putStrLn ("Downloading s3://" <> toText bucketName <> "/" <> toText objectKey <> " to " <> T.pack toFilePath)
    runResourceT . runAWST env $ do
      resp <- send (S3.getObject bucketName objectKey)
      liftResourceT (_streamBody (view S3.gorsBody resp) $$+- sinkFile toFilePath)
  where
   onServiceError :: ServiceError -> IO ()
   onServiceError se =
     case retryAfter of
        Nothing -> throwing _ServiceError se
        (Just delaySecs) -> do
          threadDelay (1000000 * delaySecs)
          downloadFileFromS3 env bucketName objectKey toFilePath retryAfter
