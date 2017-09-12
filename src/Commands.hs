{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module Commands where

import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import ADL.Config(ToolConfig(..), DeployContextFile(..))
import ADL.Release(ReleaseConfig(..))
import ADL.Core(adlFromJsonFile')
import Codec.Archive.Zip(withArchive, unpackInto)
import Control.Monad(when)
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Lens
import Data.Monoid
import Data.Conduit(runConduit, (.|), ($$), ($$+-))
import Data.Conduit.Binary(sinkFile)
import Data.Foldable(for_)
import Data.Traversable(for)
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, (</>))
import System.Posix.Files(createLink, removeLink)
import System.IO(stdout)
import System.Process(callProcess,callCommand)
import Text.Mustache(ToMustache,automaticCompile,substitute)
import Network.AWS.S3(getObject, gorsBody, BucketName(..), ObjectKey(..))
import Network.AWS.Data.Body(RsBody(..))
import Network.AWS.Data.Text(ToText(..))
import Path(Path,Abs,Dir,File,parseAbsDir,parseAbsFile)

--- Download the infrastructure context files from S3
fetchContext :: ToolConfig -> IO ()
fetchContext tcfg = do
  let cacheDir = T.unpack (tc_contextCache tcfg)
  createDirectoryIfMissing True cacheDir
  env <- mkAwsEnv
  for_ (tc_deployContextFiles tcfg) $ \cf -> do
    let cacheFilePath = cacheDir </> T.unpack (dcf_name cf)
        (bucketName,objectKey) = splitS3Path (dcf_source cf)
    downloadFileFromS3 env bucketName objectKey cacheFilePath

-- unpack a release into the specified directory, and expand any templates
unpack :: ToolConfig -> T.Text -> FilePath -> IO ()
unpack tcfg release toDir = do
  env <- mkAwsEnv
  createDirectoryIfMissing True toDir
  let (bucketName,ObjectKey bucketPath) = splitS3Path (tc_releasesS3 tcfg)
      objectKey = ObjectKey (bucketPath <> "/" <> release)
      releaseFilePath = toDir </> T.unpack release

  -- download the release zip file
  downloadFileFromS3 env bucketName objectKey releaseFilePath

  -- expand the zip file
  withArchive (toFilePath releaseFilePath) (unpackInto (toDirPath toDir))

  -- load the release metadata
  rcfg <- adlFromJsonFile' (toDir </> "release.json")

  -- load and merge the infrastructure context
  ctx <- loadMergedContext tcfg

  -- interpolate the context into each templated file
  for_ (rc_templates rcfg) $ \templatePath -> do
    expandTemplateFile ctx (toDir </> T.unpack templatePath)

select :: ToolConfig -> T.Text -> IO ()
select tcfg release = do
  let newReleaseDir = T.unpack (tc_releasesDir tcfg) </> (takeBaseName (T.unpack release))
  let currentReleaseLink = T.unpack (tc_releasesDir tcfg) </> "current"

  -- unpack new release
  createDirectoryIfMissing True newReleaseDir
  unpack tcfg release newReleaseDir

  -- Run the prestart command first, to pull/download any dependences.
  -- we do the before stopping the existing release to minimise startup time.
  -- do this first to minimise the new release startup time
  withCurrentDirectory newReleaseDir $ do
    rcfg <- adlFromJsonFile' "release.json"
    callCommand (T.unpack (rc_prestartCommand rcfg))

  -- shut down the current release if it exists
  currentExists <- doesDirectoryExist currentReleaseLink
  when currentExists $ do
    withCurrentDirectory currentReleaseLink $ do
      rcfg <- adlFromJsonFile' "release.json"
      callCommand (T.unpack (rc_stopCommand rcfg))

  -- symlink the current release to point to the new one
  when currentExists $ do
    removeLink currentReleaseLink
  createLink newReleaseDir currentReleaseLink

  -- start it
  withCurrentDirectory currentReleaseLink $ do
    rcfg <- adlFromJsonFile' "release.json"
    callCommand (T.unpack (rc_stopCommand rcfg))

downloadFileFromS3 :: Env -> BucketName -> ObjectKey -> FilePath -> IO ()
downloadFileFromS3 env bucketName  objectKey toFilePath = do
  T.putStrLn ("Downloading s3://" <> toText bucketName <> "/" <> toText objectKey <> " to " <> T.pack toFilePath)
  runResourceT . runAWST env $ do
    resp <- send (getObject bucketName objectKey)
    liftResourceT (_streamBody (view gorsBody resp) $$+- sinkFile toFilePath)

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
  etemplate <- automaticCompile [takeDirectory templatePath] templatePath
  case etemplate of
   Left err -> error (show err)
   Right template -> do
     let text = substitute template ctx
         outfile = dropExtension templatePath
     T.writeFile outfile text

mkAwsEnv :: IO Env
mkAwsEnv = do
  env0 <- newEnv Discover
  l <- newLogger Error stdout
  return (env0 & envLogger .~ l)

splitS3Path :: T.Text -> (BucketName,ObjectKey)
splitS3Path s3Path = case T.stripPrefix "s3://" s3Path of
  Nothing -> error "s3Path must start with s3://"
  (Just s) ->
    case T.breakOn "/" s of
      (_,"") -> error "s3Path must include a key"
      (b,k) -> (BucketName b,ObjectKey (T.tail k))

toDirPath :: FilePath -> Path Abs Dir
toDirPath path = case parseAbsDir path of
  Just p -> p
  Nothing -> error "Unable to parse directory"

toFilePath :: FilePath -> Path Abs File
toFilePath path = case parseAbsFile path of
  Just p -> p
  Nothing -> error "Unable to parse directory"
