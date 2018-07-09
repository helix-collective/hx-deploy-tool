{-# LANGUAGE OverloadedStrings #-}
module Commands.ProxyMode(
  showStatus,
  deploy,
  undeploy,
  connect,
  disconnect,
  slaveUpdate
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
import ADL.Config(ToolConfig(..), DeployContextFile(..), DeployMode(..), ProxyModeConfig(..), MachineLabel(..))
import ADL.State(State(..), Deploy(..))
import ADL.Types(EndPointLabel, DeployLabel)
import Commands(unpackRelease, fetchDeployContext)
import Commands.ProxyMode.Types
import Commands.ProxyMode.LocalState(localState)
import Commands.ProxyMode.RemoteState(remoteState, writeSlaveState, masterS3Path)
import Control.Concurrent(threadDelay)
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
import Types(IOR, REnv(..), getToolConfig, scopeInfo)

-- | Show the proxy system status, specifically the endpoints and live deploys.
showStatus :: Bool -> IOR ()
showStatus showSlaves = do
  pm <- getProxyModeConfig
  state <- getState
  liftIO $ printState pm state
  slaveStates <- getSlaveStates
  when showSlaves $
    liftIO $ for_ slaveStates $ \(label,slaveState) -> do
      T.putStrLn "----------------------------------------------------------------------"
      T.putStrLn ("Slave: " <> label)
      printState pm slaveState
  where
    printState pm state = do
      T.putStrLn "Endpoints:"
      for_ (pmEndPoints pm) $ \ep -> do
        let etype = case ep_etype ep of
              Ep_httpOnly -> "(" <> ep_serverName ep <> ":80)"
              Ep_httpsWithRedirect -> "(" <> ep_serverName ep <> ":80,442)"
        let connected = case SM.lookup (ep_label ep) (s_connections state) of
              Nothing -> "(not connected)"
              Just deployLabel -> deployLabel
        T.putStrLn ("  " <> ep_label ep <> ": " <> etype <> " -> " <> connected)
      T.putStrLn ""
      T.putStrLn "Deploys:"
      for_ (SM.elems (s_deploys state)) $ \d -> do
        T.putStrLn ("  " <> d_label d <> ": (localhost:" <> showText (d_port d) <> ")")

-- | Create and start a deployment (if it's not already running)
deploy :: T.Text -> IOR ()
deploy release = do
  scopeInfo ("Creating deploy " <> release) $ do
    pm <- getProxyModeConfig
    tcfg <- getToolConfig
    state <- getState
    port <- liftIO $ allocatePort pm state
    updateState (nextState (createDeploy port))
  where
    createDeploy port = (CreateDeploy (Deploy release release port))

-- | Stop and remove a deployment
undeploy :: T.Text -> IOR ()
undeploy release = do
  scopeInfo ("Removing deploy " <> release) $ do
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
      slaveUpdate_
      liftIO $ do
        t1 <- getCurrentTime
        let delay = floor (toRational (diffUTCTime (addUTCTime (fromIntegral repeat) t0) t1))
        when (delay > 0) (threadDelay (delay * 1000000))
      loop

slaveUpdate_:: IOR ()
slaveUpdate_ = do
  pm <- getProxyModeConfig
  let remoteStateS3 = case pm_remoteStateS3 pm of
        Nothing -> error "Remote state is not configured"
        (Just s3Path) -> s3Path
  scopeInfo ("Fetching state from " <> masterS3Path remoteStateS3) $ do
    state <- sa_get (remoteState remoteStateS3)
    sa_update localState (const state)
    label <- getSlaveLabel
    writeSlaveState remoteStateS3 label state

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

getSlaveStates :: IOR [(T.Text, State)]
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


getSlaveLabel :: IOR T.Text
getSlaveLabel = do
  pm <- getProxyModeConfig
  case pm_slaveLabel pm of
    MachineLabel_label label -> return label
    MachineLabel_ec2InstanceId -> liftIO $ do
      mgr <- HC.newManager HC.defaultManagerSettings
      bs <- EC2M.metadata mgr EC2M.InstanceId
      return (T.decodeUtf8 bs)
