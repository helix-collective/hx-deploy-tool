{-# LANGUAGE OverloadedStrings #-}
module Commands.ProxyMode(
  showStatus,
  deploy,
  undeploy,
  connect,
  disconnect
  ) where

import qualified ADL.Core.StringMap as SM
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Set as S

import ADL.Config(EndPoint(..), EndPointType(..))
import ADL.Core(adlFromJsonFile', adlToJsonFile)
import ADL.Release(ReleaseConfig(..))
import ADL.Config(ToolConfig(..), DeployContextFile(..))
import ADL.State(State(..), Deploy(..))
import ADL.Types(EndPointLabel, DeployLabel)
import Commands(unpackRelease, fetchDeployContext)
import Control.Monad.Reader(ask)
import Control.Monad.IO.Class
import Data.List(find)
import Data.Maybe(catMaybes)
import Data.Foldable(for_)
import Data.Monoid
import Data.Word
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory, removeDirectoryRecursive)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, (</>))
import System.Process(callCommand)
import Types(IOR, REnv(..), getToolConfig, scopeInfo)

-- | Show the proxy system status, specifically the endpoints and live deploys.
showStatus :: IOR ()
showStatus = do
  checkProxyEnabled
  state <- getState
  tcfg <- getToolConfig
  liftIO $ do
    T.putStrLn "Endpoints:"
    for_ (tcEndPoints tcfg) $ \ep -> do
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
    checkProxyEnabled
    tcfg <- getToolConfig
    state <- getState
    fetchDeployContext Nothing
    port <- liftIO $ allocatePort tcfg state
    updateState (nextState (createDeploy port))
  where
    createDeploy port = (CreateDeploy (Deploy release release port))

-- | Stop and remove a deployment
undeploy :: T.Text -> IOR ()
undeploy release = do
  scopeInfo ("Removing deploy " <> release) $ do
    checkProxyEnabled
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
    checkProxyEnabled
    tcfg <- getToolConfig
    state <- getState
    case SM.lookup deployLabel (s_deploys state) of
      Nothing -> error (T.unpack ("no deploy called " <> deployLabel))
      Just deploy -> return ()
    case SM.lookup endPointLabel (tc_endPoints tcfg) of
      Nothing -> error (T.unpack ("no endpoint called " <> endPointLabel))
      Just endPoint -> return ()
    updateState (\s -> s{s_connections=SM.insert endPointLabel deployLabel (s_connections s)})

-- | Disconnect an endpoint
disconnect :: T.Text -> IOR ()
disconnect endPointLabel = do
  scopeInfo ("Disconnecting endpoint " <> endPointLabel) $ do
    checkProxyEnabled
    tcfg <- getToolConfig
    state <- getState
    case SM.lookup endPointLabel (tc_endPoints tcfg) of
      Nothing -> error (T.unpack ("no endpoint called " <> endPointLabel))
      Just endPoint -> return ()
    updateState (\s -> s{s_connections=SM.delete endPointLabel (s_connections s)})

----------------------------------------------------------------------
-- The code below implements state changes of the proxy, with their
-- associated side effects.
--
-- The code has been structured to support the future use case where
-- we store a single master copy of the target proxy state. EC2
-- instances will periodically poll this target state, and work out
-- the actions required to update their local state accordingly.

-- | Execute all necessary actions (with their effects) to update the proxy state.
updateState :: (State -> State) -> IOR ()
updateState newStateFn = do
  stateFile <- getStateFile
  state <- getState
  tcfg <- getToolConfig
  let newState = newStateFn state
  mapM_ (runAction stateFile) (stateUpdateActions (SM.toMap (tc_endPoints tcfg)) state newState)
  where
    runAction stateFile action = do
      state <- getState
      executeAction action
      liftIO $ adlToJsonFile stateFile (nextState action state)


-- | The actions we can apply to change the state
data StateAction
  = CreateDeploy Deploy
  | DestroyDeploy Deploy
  | SetEndPoints [(EndPoint,Deploy)]


-- | Compute the difference between oldState and newState, and express this as a list
-- of actions to update the old state
stateUpdateActions :: M.Map EndPointLabel EndPoint -> State -> State -> [StateAction]
stateUpdateActions endPointMap oldState newState
  =  map CreateDeploy deploysToCreate
  <> concatMap updateDeploy deploysToUpdate
  <> if newEndPoints /= oldEndPoints then [SetEndPoints newEndPoints] else []
  <> map DestroyDeploy deploysToDestroy
  where
    oldDeploys = SM.toMap (s_deploys oldState)
    newDeploys = SM.toMap (s_deploys newState)
    oldEndPoints = (catMaybes . map getEndpointDeploy . SM.toList) (s_connections oldState)
    newEndPoints = (catMaybes . map getEndpointDeploy . SM.toList) (s_connections newState)
    deploysToCreate = M.elems (M.difference newDeploys oldDeploys)
    deploysToUpdate = filter (\(d1,d2) -> d1 /= d2) (M.elems (M.intersectionWith (\d1 d2 -> (d1,d2)) oldDeploys newDeploys))
    deploysToDestroy = M.elems (M.difference oldDeploys newDeploys)
    updateDeploy (d1,d2) = [DestroyDeploy d1, CreateDeploy d2]
    getEndpointDeploy :: (EndPointLabel,DeployLabel) -> Maybe (EndPoint,Deploy)
    getEndpointDeploy (eplabel, dlabel) = do
      endPoint <- find (\ep -> ep_label ep == eplabel)  endPointMap
      deploy <- find (\d -> d_label d == dlabel) (SM.toMap (s_deploys newState))
      return (endPoint,deploy)


-- | Update the state with the effect of an action
nextState :: StateAction -> State -> State
nextState (CreateDeploy d) s = s{s_deploys=SM.insert (d_label d) d (s_deploys s)}
nextState (DestroyDeploy d) s = s{s_deploys= SM.delete (d_label d) (s_deploys s)}
nextState (SetEndPoints eps) s = s{s_connections=SM.fromList [ (ep_label ep, d_label d) | (ep,d) <- eps ]}


-- | Execute the effects of a single action action
executeAction :: StateAction -> IOR ()

executeAction (CreateDeploy d) = do
  scopeInfo "execute CreateDeploy" $ do
    tcfg <- getToolConfig
    let deployDir = T.unpack (tc_releasesDir tcfg) </> (takeBaseName (T.unpack (d_release d)))
    liftIO $ createDirectoryIfMissing True deployDir
    unpackRelease (contextWithLocalPorts tcfg (d_port d)) (d_release d) deployDir

    -- Start it up
    rcfg <- getReleaseConfig deployDir
    scopeInfo "running prestart script" $ callCommandInDir deployDir (rc_prestartCommand rcfg)
    scopeInfo "running start script" $ callCommandInDir deployDir (rc_startCommand rcfg)

executeAction (DestroyDeploy d) = do
  scopeInfo "execute DestroyDeploy" $ do
    tcfg <- getToolConfig
    let deployDir = T.unpack (tc_releasesDir tcfg) </> (takeBaseName (T.unpack (d_release d)))
    rcfg <- getReleaseConfig deployDir
    scopeInfo "running stop script" $ callCommandInDir deployDir (rc_stopCommand rcfg)
    scopeInfo "removing directory" $ liftIO $ removeDirectoryRecursive deployDir

executeAction (SetEndPoints eps) = do
  scopeInfo "execute SetEndPoints" $ do
    proxyDir <- getProxyDir
    scopeInfo "writing proxy config files" $ liftIO $ do
      writeProxyDockerCompose (proxyDir </> "docker-compose.yml")
      writeNginxConfig (proxyDir </> "nginx.conf") eps
    callCommandInDir proxyDir "docker-compose up -d"
    callCommandInDir proxyDir "docker kill --signal=SIGHUP frontendproxy"

getState :: IOR State
getState = do
  stateFile <- getStateFile
  exists <- liftIO $ doesFileExist stateFile
  case exists of
    True -> liftIO $ adlFromJsonFile' stateFile
    False -> return emptyState
  where
    emptyState = State mempty mempty

getProxyDir :: IOR FilePath
getProxyDir = do
  tcfg <- getToolConfig
  let proxyDir = T.unpack (tc_releasesDir tcfg) </> "frontend-proxy"
  liftIO $ createDirectoryIfMissing True proxyDir
  return proxyDir

getStateFile :: IOR FilePath
getStateFile = do
  proxyDir <- getProxyDir
  return (proxyDir </> "state.json")


writeProxyDockerCompose :: FilePath -> IO ()
writeProxyDockerCompose path = T.writeFile path (T.intercalate "\n" lines)
  where
    lines =
      [ "version: '2'"
      , "services:"
      , "  nginx:"
      , "    container_name: frontendproxy"
      , "    image: nginx:1.13.0"
      , "    network_mode: host"
      , "    volumes:"
      , "      - ./nginx.conf:/etc/nginx/nginx.conf"
      , "      - /etc/letsencrypt:/etc/letsencrypt"
      ]

writeNginxConfig :: FilePath -> [(EndPoint,Deploy)] -> IO ()
writeNginxConfig path eps = T.writeFile path (T.intercalate "\n" lines)
  where
    lines =
      [ "user  nginx;"
      , "worker_processes  1;"
      , ""
      , "error_log  /dev/stderr;"
      , "pid        /var/run/nginx.pid;"
      , ""
      , "events {"
      , "worker_connections  1024;"
      , "}"
      , ""
      , "http {"
      , "  include       /etc/nginx/mime.types;"
      , "  default_type  application/octet-stream;"
      , ""
      , "  access_log  /dev/stdout;"
      , "  error_log   /dev/stderr;"
      , ""
      , "  sendfile        on;"
      , ""
      , "  keepalive_timeout  65;"
      , ""
      , "  proxy_buffering on;"
      , "  proxy_temp_path proxy_temp 1 2;"
      , ""
      , "  charset utf-8;"
      , ""
      ] <>
      concat (map serverBlock eps) <>
      [ "}"
      ]
    serverBlock (ep@EndPoint{ep_etype=Ep_httpOnly},d) =
      [ "  server {"
      , "    listen 80;"
      , "    server_name " <> ep_serverName ep <> ";"
      , "    location / {"
      , "      proxy_pass http://localhost:" <> showText (d_port d) <> "/;"
      , "    }"
      , "  }"
      ]
    serverBlock (ep@EndPoint{ep_etype=Ep_httpsWithRedirect},d) =
      [ "  server {"
      , "    listen 80;"
      , "    server_name " <> ep_serverName ep <> ";"
      , "    return 301 https://$server_name$request_uri;"
      , "  }"
      , "  server {"
      , "    listen       443 ssl;"
      , "    server_name " <> ep_serverName ep <> ";"
      , "    ssl_certificate " <> ep_sslCertDir ep <> "/fullchain.pem;"
      , "    ssl_certificate_key " <> ep_sslCertDir ep <> "/privkey.pem;"
      , "    location / {"
      , "      proxy_pass http://localhost:" <> showText (d_port d) <> "/;"
      , "    }"
      , "  }"
      ]

-- | Allocate an open port in the configured range
allocatePort :: ToolConfig -> State -> IO Word32
allocatePort tcfg state = case S.lookupMin ports of
  Just port -> return port
  Nothing -> error "no more ports available"
  where
    ports = S.difference (S.fromList [minPort..maxPort-1]) (S.fromList (map d_port (M.elems (SM.toMap (s_deploys state)))))
    (minPort,maxPort) = tc_dynamicPortRange tcfg

checkProxyEnabled :: IOR ()
checkProxyEnabled = do
  tcfg <- getToolConfig
  case (tcEndPoints tcfg) of
    [] -> error "Proxy disabled, as no endpoints specified in the configuration"
    _ -> return ()

-- Extend the release template context with port variables,
-- which include the http port where the deploy will make
-- itself available, and some extra ones for general purpose
-- ad-hoc use in the deploy.

contextWithLocalPorts :: ToolConfig -> Word32 -> JS.Value -> JS.Value
contextWithLocalPorts tcfg port (JS.Object o) = JS.Object (HM.insert "ports" (JS.toJSON ports) o)
  where
    ports :: M.Map T.Text Word32
    ports = M.fromList
      [ ("http",port)
      , ("extra1", extra 1)
      , ("extra2", extra 2)
      , ("extra3", extra 3)
      , ("extra4", extra 4)
      ]
    extra n = port + n * (maxPort - minPort)
    (minPort,maxPort) = tc_dynamicPortRange tcfg
contextWithLocalPorts _ _ ctx = ctx

tcEndPoints :: ToolConfig -> [EndPoint]
tcEndPoints tcfg = SM.elems (tc_endPoints tcfg)

showText :: Show a => a -> T.Text
showText = T.pack . show

callCommandInDir :: FilePath -> T.Text -> IOR ()
callCommandInDir inDir cmd = do
  scopeInfo ("Running: " <> cmd)  $ do
    liftIO $ withCurrentDirectory inDir $ callCommand (T.unpack cmd)

getReleaseConfig :: FilePath -> IOR ReleaseConfig
getReleaseConfig deployDir = do
  liftIO $ adlFromJsonFile' (deployDir </> "release.json")
