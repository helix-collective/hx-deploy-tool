{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, Rank2Types #-}
module Commands where

import qualified Data.Map as M
import qualified Data.Set as S
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
import qualified Text.Mustache as TM
import qualified Text.Mustache.Types as TM
import qualified Commands.ProxyMode as P
import qualified ADL.Sys.Types as ST

import ADL.Config(ToolConfig(..), DeployMode(..), ProxyModeConfig(..), DynamicConfigOptions(..), DynamicJsonSource(..), JsonSource(..))
import ADL.Types(DynamicConfigName, StringKeyMap, DynamicConfigMode)
import ADL.Release(ReleaseConfig(..))
import ADL.Core(adlFromJsonFile')
import Blobs(releaseBlobStore, BlobStore(..))
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
import Data.Foldable(for_)
import Data.Time.Clock.POSIX(getCurrentTime)
import Data.Traversable(for)
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, (</>))
import System.Posix.Files(createSymbolicLink, removeLink)
import System.IO(stdout, withFile, hIsEOF, IOMode(..))
import System.Process(callCommand)
import Path(Path,Abs,Dir,File,parseAbsDir,parseAbsFile)
import Types(IOR, REnv(..), getToolConfig, scopeInfo)
import Util(unpackRelease, fetchConfigContext, jsrcLabel)
import Util.Aws(mkAwsEnv)
import Commands.ProxyMode.LocalState(nginxConfTemplate)

-- Make the specified release the live release, replacing any existing release.
createAndStart :: T.Text -> IOR ()
createAndStart release = do
  tcfg <- getToolConfig
  case tc_deployMode tcfg of
    DeployMode_noproxy -> startNoProxy release
    _ -> P.createAndStart release

startNoProxy :: T.Text -> IOR ()
startNoProxy release = do
  scopeInfo ("Selecting active release " <> release) $ do
    tcfg <- getToolConfig
    let newReleaseDir = T.unpack (tc_deploysDir tcfg) </> (takeBaseName (T.unpack release))
    let currentReleaseLink = T.unpack (tc_deploysDir tcfg) </> "current"

    -- Fetch the context in case it has been updated
    fetchConfigContext Nothing

    liftIO $ createDirectoryIfMissing True newReleaseDir

    -- unpack new release
    unpackRelease id release newReleaseDir

    -- Run the prestart command first, to pull/download any dependences.
    -- we do the before stopping the existing release to minimise startup time.
    -- do this first to minimise the new release startup time
    scopeInfo "Running prestart script" $ liftIO $ do
      withCurrentDirectory newReleaseDir $ do
        rcfg <- adlFromJsonFile' "release.json"
        callCommand (T.unpack (rc_prestartCommand rcfg))

    currentExists <- liftIO $ doesDirectoryExist currentReleaseLink
    when currentExists $ do
      scopeInfo "Stopping existing release" $ liftIO $ do
        withCurrentDirectory currentReleaseLink $ do
          rcfg <- adlFromJsonFile' "release.json"
          callCommand (T.unpack (rc_stopCommand rcfg))

    scopeInfo "Symlinking new release" $ liftIO $ do
      when currentExists $ removeLink currentReleaseLink
      createSymbolicLink newReleaseDir currentReleaseLink

    scopeInfo "Starting new release" $ liftIO $ do
      withCurrentDirectory currentReleaseLink $ do
        rcfg <- adlFromJsonFile' "release.json"
        callCommand (T.unpack (rc_startCommand rcfg))

-- Stop the specified deployment.
stopDeploy :: T.Text -> IOR ()
stopDeploy deploy = do
  tcfg <- getToolConfig
  case tc_deployMode tcfg of
    DeployMode_noproxy -> stopNoProxy deploy
    _ -> P.stopAndRemove deploy

stopNoProxy :: T.Text -> IOR ()
stopNoProxy deploy = do
  tcfg <- getToolConfig
  --let newReleaseDir = T.unpack (tc_deploysDir tcfg) </> (takeBaseName (T.unpack deploy))
  let currentReleaseLink = T.unpack (tc_deploysDir tcfg) </> "current"
  currentExists <- liftIO $ doesDirectoryExist currentReleaseLink
  scopeInfo ("Stopping active deployment " <> deploy) $ liftIO $ do
    when currentExists $ do
      withCurrentDirectory currentReleaseLink $ liftIO $ do
          rcfg <- adlFromJsonFile' "release.json"
          callCommand (T.unpack (rc_stopCommand rcfg))



  scopeInfo ("Removing Symlink for deployment") $ liftIO $ do
    when currentExists $ removeLink currentReleaseLink


-- List the releases available for installation
listReleases :: IOR ()
listReleases = do
  bs <- releaseBlobStore
  liftIO $ do
    names <- bs_names bs
    mapM_ T.putStrLn names

-- Output the command line to docker login to access the default
-- repository
awsDockerLoginCmd :: IOR ()
awsDockerLoginCmd = do
  tcfg <- getToolConfig
  env <- mkAwsEnv
  liftIO $ do
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
showLog :: IOR ()
showLog = do
  tcfg <- getToolConfig
  let logFile = T.unpack (tc_logFile tcfg)
  liftIO $ withFile logFile ReadMode $ \h -> nextLine h
  where
    nextLine h = do
      eof <- hIsEOF h
      when (not eof) $ do
        T.hGetLine h >>= T.putStrLn
        nextLine h

-- dump the default nginx.conf mustache template
showDefaultNginxConfig :: IO ()
showDefaultNginxConfig = do
  T.putStrLn nginxConfTemplate

type DynamicConfigSources = (StringKeyMap DynamicConfigName DynamicJsonSource)

listDynamicConfigOptions :: DynamicConfigSources -> DynamicConfigOptions
listDynamicConfigOptions dcsrcs = SM.fromList (M.toList (M.map dynamicJsonSourceToSet (SM.toMap dcsrcs)))
  where
    dynamicJsonSourceToSet :: DynamicJsonSource -> ST.Set DynamicConfigMode
    dynamicJsonSourceToSet x = M.keysSet (SM.toMap( djsrc_modes x))

getConfigOptionsText :: DynamicConfigOptions -> [T.Text]
getConfigOptionsText dcopts = map dcNameModesText (SM.toList dcopts)
  where
    dcNameModesText :: (T.Text, ST.Set DynamicConfigMode) -> T.Text
    dcNameModesText tupl = T.intercalate (T.pack ": ") [T.justifyLeft 10 ' ' (fst tupl), dcModesText (snd tupl)]

    dcModesText :: (ST.Set DynamicConfigMode) -> T.Text
    dcModesText setdcm = T.intercalate (T.pack ", ") (S.toList setdcm)

printDynamicConfigOptions :: DynamicConfigOptions -> IO ()
printDynamicConfigOptions dcopts = liftIO $ do
  mapM_ T.putStrLn (getConfigOptionsText dcopts)

printDynamicConfigOptionsSingle :: DynamicConfigName -> DynamicConfigOptions -> IO ()
printDynamicConfigOptionsSingle dcname dcopts = liftIO $ do
  mapM_ T.putStrLn (getConfigOptionsText (filterByConfigName dcname dcopts))
  where
    filterByConfigName :: DynamicConfigName -> DynamicConfigOptions -> DynamicConfigOptions
    filterByConfigName dcname dcopts = SM.fromList (M.toList (M.filterWithKey (\k _ -> k == dcname) (SM.toMap dcopts)))

listConfigsModes :: IOR ()
listConfigsModes = do
  tcfg <- getToolConfig
  liftIO $ printDynamicConfigOptions (listDynamicConfigOptions (tc_dynamicConfigSources tcfg))

showConfigModes :: DynamicConfigName -> IOR ()
showConfigModes dcname = do
  tcfg <- getToolConfig
  liftIO $ printDynamicConfigOptionsSingle dcname (listDynamicConfigOptions (tc_dynamicConfigSources tcfg))
