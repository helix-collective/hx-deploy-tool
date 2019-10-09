{-# LANGUAGE OverloadedStrings #-}
module Commands.ProxyMode(
  showStatus,
  createAndStart,
  stopAndRemove,
  connect,
  disconnect,
  slaveFlush,
  slaveUpdate,
  restartProxy,
  generateSslCertificate,
  ) where

import qualified ADL.Core.StringMap as SM
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Set as S
import qualified Network.HTTP.Client as HC
import qualified Network.AWS.EC2.Metadata as EC2M

import ADL.Config(EndPoint(..), EndPointType(..))
import ADL.Core(adlFromJsonFile', adlToJsonFile)
import ADL.Release(ReleaseConfig(..))
import ADL.Config(ToolConfig(..), DeployMode(..), ProxyModeConfig(..), MachineLabel(..))
import ADL.State(State(..), Deploy(..), SlaveState(..), SlaveStatus(..))
import ADL.Types(EndPointLabel, DeployLabel)
import Util(unpackRelease,fetchConfigContext, checkReleaseExists)
import Commands.ProxyMode.Types
import Commands.ProxyMode.LocalState(localState, restartLocalProxy, generateLocalSslCertificate)
import Commands.ProxyMode.RemoteState(remoteState, writeSlaveState, masterS3Path, flushSlaveStates)
import Control.Concurrent(threadDelay)
import Control.Exception(throwIO, SomeException)
import Control.Monad.Catch(catch, handle)
import Control.Monad.Reader(ask)
import Control.Monad.IO.Class
import Control.Monad(when)
import Data.List(find)
import Data.Maybe(catMaybes)
import Data.Foldable(for_)
import Data.Monoid
import Data.Word
import Data.Time.Clock(addUTCTime,diffUTCTime,getCurrentTime)
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory, removeDirectoryRecursive)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, (</>))
import System.Process(callCommand)
import Types(IOR, REnv(..), getToolConfig, scopeInfo, flushlog, info, lerror)

-- | Show the proxy system status, specifically the endpoints and live deploys.
showStatus :: Bool -> IOR ()
showStatus showSlaves = do
  pm <- getProxyModeConfig
  state <- getState
  liftIO $ printState pm state
  when showSlaves $ do
    slaveStates <- getSlaveStates
    liftIO $ for_ slaveStates $ \(label,slaveState) -> do
      T.putStrLn "----------------------------------------------------------------------"
      T.putStrLn ("Slave: " <> label)
      case slaveState_status (lm_value slaveState) of
        SlaveStatus_ok -> T.putStrLn ("Status: OK")
        SlaveStatus_error emsg -> T.putStrLn ("Status: Error (" <> emsg <> ")")
      for_ (lm_modifiedAt slaveState) $ \lm ->
         T.putStrLn ("Updated: " <> (T.pack (show lm)))
      printState pm (slaveState_state (lm_value slaveState))
  where
    printState pm state = do
      T.putStrLn "Endpoints:"
      for_ (pmEndPoints pm) $ \(eplabel,ep) -> do
        let etype = case ep_etype ep of
              Ep_httpOnly -> "(" <> T.intercalate ", " [sn <> ":80" |sn <- ep_serverNames ep ] <> ")"
              Ep_httpsWithRedirect _ -> "(" <> T.intercalate ", " [sn <> ":80,443" |sn <- ep_serverNames ep ] <> ")"
        let connected = case SM.lookup eplabel (s_connections state) of
              Nothing -> "(not connected)"
              Just deployLabel -> deployLabel
        T.putStrLn ("  " <> eplabel <> ": " <> etype <> " -> " <> connected)
      T.putStrLn ""
      T.putStrLn "Deploys:"
      for_ (SM.elems (s_deploys state)) $ \d -> do
        T.putStrLn ("  " <> d_label d <> ": (localhost:" <> showText (d_port d) <> ")")

-- | Create and start a deployment (if it's not already running)
createAndStart :: T.Text -> IOR ()
createAndStart release = do
  checkReleaseExists release
  scopeInfo ("Create and start deploy " <> release) $ do
    pm <- getProxyModeConfig
    tcfg <- getToolConfig
    state <- getState
    port <- liftIO $ allocatePort pm state
    dcfgmodes <- return SM.empty
    updateState (nextState (createDeploy port dcfgmodes))
  where
    createDeploy port dcfgmodes = (CreateDeploy (Deploy release release port dcfgmodes))

-- | Stop and remove a deployment
stopAndRemove :: T.Text -> IOR ()
stopAndRemove release = do
  scopeInfo ("Stop and remove deploy " <> release) $ do
    pm <- getProxyModeConfig
    state <- getState
    deploy <- case SM.lookup release (s_deploys state) of
      Nothing -> error (T.unpack ("no deploy called " <> release))
      Just deploy -> return deploy
    case find ((==release).snd) (SM.toList (s_connections state)) of
      Just (endpointLabel,_) -> error (T.unpack ("deploy is connected to " <> endpointLabel))
      Nothing -> return ()
    updateState (nextState (DestroyDeploy deploy))

-- | Connect an endpoint to a running deployment
connect :: T.Text -> T.Text -> IOR ()
connect endPointLabel deployLabel = do
  scopeInfo ("Connecting endpoint " <> endPointLabel <> " to " <> deployLabel) $ do
    pm <- getProxyModeConfig
    tcfg <- getToolConfig
    state <- getState
    case SM.lookup deployLabel (s_deploys state) of
      Nothing -> error (T.unpack ("no deploy called " <> deployLabel))
      Just deploy -> return ()
    case SM.lookup endPointLabel (pm_endPoints pm) of
      Nothing -> error (T.unpack ("no endpoint called " <> endPointLabel))
      Just endPoint -> return ()
    updateState (\s -> s{s_connections=SM.insert endPointLabel deployLabel (s_connections s)})

-- | Disconnect an endpoint
disconnect :: T.Text -> IOR ()
disconnect endPointLabel = do
  scopeInfo ("Disconnecting endpoint " <> endPointLabel) $ do
    pm <- getProxyModeConfig
    tcfg <- getToolConfig
    state <- getState
    case SM.lookup endPointLabel (pm_endPoints pm) of
      Nothing -> error (T.unpack ("no endpoint called " <> endPointLabel))
      Just endPoint -> return ()
    updateState (\s -> s{s_connections=SM.delete endPointLabel (s_connections s)})

-- | Update local state to reflect the master state from S3
slaveUpdate :: Maybe Int -> IOR ()
slaveUpdate Nothing = slaveUpdate_
slaveUpdate (Just repeat) = loop
  where
    loop = do
      t0 <- liftIO $ getCurrentTime
      catch slaveUpdate_ ehandler
      flushlog
      liftIO $ do
        t1 <- getCurrentTime
        let delay = floor (toRational (diffUTCTime (addUTCTime (fromIntegral repeat) t0) t1))
        when (delay > 0) (threadDelay (delay * 1000000))
      loop

    ehandler e = do
      lerror ("Exception: " <> T.pack (show (e::SomeException)))
      info "(slave state not updated)"

slaveUpdate_:: IOR ()
slaveUpdate_ = do
  pm <- getProxyModeConfig
  let remoteStateS3 = case pm_remoteStateS3 pm of
        Nothing -> error "Remote state is not configured"
        (Just s3Path) -> s3Path
  scopeInfo ("Fetching state from " <> masterS3Path remoteStateS3) $ do
    state <- sa_get (remoteState remoteStateS3)
    label <- getSlaveLabel
    handle (ehandler remoteStateS3 label) $ do
      sa_update localState (const state)
      writeSlaveState remoteStateS3 label (SlaveState SlaveStatus_ok state)
  where
    ehandler remoteStateS3 label e = do
      let emsg = T.pack (show (e::SomeException))
      existingState <- sa_get localState
      writeSlaveState remoteStateS3 label (SlaveState (SlaveStatus_error emsg) existingState)
      liftIO $ throwIO e

-- Flash slave state from S3 that is more than 5 minutes old
slaveFlush :: IOR ()
slaveFlush = do
  pm <- getProxyModeConfig
  slaveStates <- getSlaveStates
  now <- liftIO $ getCurrentTime
  let notUpdatedSince = addUTCTime (fromIntegral (-300)) now
  for_ (pm_remoteStateS3 pm) $ \s3Path -> do
    flushSlaveStates notUpdatedSince s3Path
    return ()

-- | Allocate an open port in the configured range
allocatePort :: ProxyModeConfig -> State -> IO Word32
allocatePort pm state = case S.lookupMin ports of
  Just port -> return port
  Nothing -> error "no more ports available"
  where
    ports = S.difference (S.fromList [minPort..maxPort-1]) (S.fromList (map d_port (M.elems (SM.toMap (s_deploys state)))))
    (minPort,maxPort) = pm_dynamicPortRange pm

getState :: IOR State
getState = do
  pm <- getProxyModeConfig
  case pm_remoteStateS3 pm of
    Nothing -> sa_get localState
    (Just s3Path) -> sa_get (remoteState s3Path)

getSlaveStates :: IOR [(T.Text, LastModified SlaveState)]
getSlaveStates = do
  pm <- getProxyModeConfig
  case pm_remoteStateS3 pm of
    Nothing -> sa_getSlaves localState
    (Just s3Path) -> sa_getSlaves (remoteState s3Path)

updateState :: (State -> State) -> IOR ()
updateState modf = do
  pm <- getProxyModeConfig
  case pm_remoteStateS3 pm of
    Nothing -> sa_update localState modf
    (Just s3Path) -> sa_update (remoteState s3Path) modf

restartProxy :: IOR ()
restartProxy = do
  pm <- getProxyModeConfig
  case pm_remoteStateS3 pm of
    Nothing -> restartLocalProxy
    _ -> return ()

generateSslCertificate :: IOR ()
generateSslCertificate = do
  pm <- getProxyModeConfig
  case pm_remoteStateS3 pm of
    Nothing -> generateLocalSslCertificate
    _ -> return ()


getSlaveLabel :: IOR T.Text
getSlaveLabel = do
  pm <- getProxyModeConfig
  case pm_slaveLabel pm of
    MachineLabel_label label -> return label
    MachineLabel_ec2InstanceId -> liftIO $ do
      mgr <- HC.newManager HC.defaultManagerSettings
      bs <- EC2M.metadata mgr EC2M.InstanceId
      return (T.decodeUtf8 bs)
