{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module Commands where

import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Text.Mustache as TM
import qualified Text.Mustache.Types as TM
import qualified Network.AWS.ECR as ECR
import qualified Network.AWS.S3 as S3

import ADL.Config(ToolConfig(..), DeployContextFile(..))
import ADL.Release(ReleaseConfig(..))
import ADL.Core(adlFromJsonFile')
import Codec.Archive.Zip(withArchive, unpackInto)
import Control.Concurrent(threadDelay)
import Control.Exception.Lens
import Control.Monad(when)
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Monad.Catch
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

--- Download the infrastructure context files from S3
fetchContext :: ToolConfig -> Maybe Int -> IO ()
fetchContext tcfg retryAfter = do
  let cacheDir = T.unpack (tc_contextCache tcfg)
  createDirectoryIfMissing True cacheDir
  env <- mkAwsEnv
  for_ (tc_deployContextFiles tcfg) $ \cf -> do
    let cacheFilePath = cacheDir </> T.unpack (dcf_name cf)
        (bucketName,objectKey) = splitS3Path (dcf_source cf)
    downloadFileFromS3 env bucketName objectKey cacheFilePath retryAfter

-- unpack a release into the specified directory, and expand any templates
unpack :: ToolConfig -> T.Text -> FilePath -> IO ()
unpack tcfg release toDir = do
  env <- mkAwsEnv
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
  ctx <- loadMergedContext tcfg


  -- interpolate the context into each templated file
  for_ (rc_templates rcfg) $ \templatePath -> do
    expandTemplateFile ctx (toDir </> T.unpack templatePath)

-- Make the specified release the live release, replacing any existing release.
select :: ToolConfig -> T.Text -> IO ()
select tcfg release = do
  let newReleaseDir = T.unpack (tc_releasesDir tcfg) </> (takeBaseName (T.unpack release))
  let currentReleaseLink = T.unpack (tc_releasesDir tcfg) </> "current"

  -- Log the request
  logMessage tcfg ("Selecting release " <> release)

  -- Fetch the context in case it has been updated
  fetchContext tcfg Nothing

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
  createSymbolicLink newReleaseDir currentReleaseLink

  -- start it
  withCurrentDirectory currentReleaseLink $ do
    rcfg <- adlFromJsonFile' "release.json"
    callCommand (T.unpack (rc_startCommand rcfg))

-- List the releases available for installation
listReleases :: ToolConfig -> IO ()
listReleases tcfg = do
  env <- mkAwsEnv
  let (bucketName,S3.ObjectKey bucketPath) = splitS3Path (tc_releasesS3 tcfg)
      listObjectReq = set S3.loPrefix (Just bucketPath) (S3.listObjects bucketName)
  runResourceT . runAWST env $ do
    objects <- paginate listObjectReq $$ CL.foldMap (view S3.lorsContents)
    let sortedObjects = reverse (sortOn (view S3.oLastModified) objects)
    for_ sortedObjects $ \object ->
      case (view S3.oKey object ^? S3.keyName '/') of
        Nothing -> return ()
        (Just package) -> liftIO (T.putStrLn package)

-- Output the command line to docker login to access the default
-- repository
awsDockerLoginCmd :: ToolConfig -> IO ()
awsDockerLoginCmd tcfg = do
  env <- mkAwsEnv
  runResourceT . runAWST env $ do
    resp <- send ECR.getAuthorizationToken
    case view ECR.gatrsAuthorizationData resp of
      [authData] -> do
        let rawtoken = fromMaybe ("error no token in authdata") (view ECR.adAuthorizationToken authData)
        let rawendpoint = fromMaybe ("error no endpointing in authdata") (view ECR.adProxyEndpoint authData)
        liftIO $ T.putStrLn (loginCmd rawtoken rawendpoint)
      _ -> error ("Expected authdata for a single registry")
  where
    loginCmd :: T.Text -> T.Text -> T.Text
    loginCmd rawtoken rawendpoint = "docker login -u AWS -p " <> password <> " " <> endpoint
      where
        password = case (T.stripPrefix "AWS:". T.decodeUtf8 . B64.decodeLenient . T.encodeUtf8) rawtoken of
          Nothing -> error ("Unable to decode docker password")
          (Just password) -> password

        endpoint = case T.stripPrefix "https://" rawendpoint of
          Nothing -> endpoint
          (Just endpoint) -> endpoint

-- dump the log file
showLog :: ToolConfig -> IO ()
showLog tcfg = do
  let logFile = T.unpack (tc_logFile tcfg)
  withFile logFile ReadMode $ \h -> nextLine h
  where
    nextLine h = do
      eof <- hIsEOF h
      when (not eof) $ do
        T.hGetLine h >>= T.putStrLn
        nextLine h

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

mkAwsEnv :: IO Env
mkAwsEnv = do
  env0 <- newEnv Discover
  logger <- newLogger Error stdout
  return (env0 & envLogger .~ logger)

splitS3Path :: T.Text -> (S3.BucketName,S3.ObjectKey)
splitS3Path s3Path = case T.stripPrefix "s3://" s3Path of
  Nothing -> error "s3Path must start with s3://"
  (Just s) ->
    case T.breakOn "/" s of
      (_,"") -> error "s3Path must include a key"
      (b,k) -> (S3.BucketName b,S3.ObjectKey (T.tail k))

toDirPath :: FilePath -> Path Abs Dir
toDirPath path = case parseAbsDir path of
  Just p -> p
  Nothing -> error "Unable to parse directory"

toFilePath :: FilePath -> Path Abs File
toFilePath path = case parseAbsFile path of
  Just p -> p
  Nothing -> error "Unable to parse directory"

-- crude and slow function to write a line to the log file
logMessage :: ToolConfig -> T.Text -> IO ()
logMessage tcfg message = do
  now <- getCurrentTime
  let logFile = T.unpack (tc_logFile tcfg)
      ltext = T.pack (show now) <> ": " <> message <> "\n"
  createDirectoryIfMissing True (takeDirectory logFile)
  T.appendFile logFile ltext
