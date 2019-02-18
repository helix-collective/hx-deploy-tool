{-# LANGUAGE OverloadedStrings #-}
module Commands.ProxyMode.LocalState(
  localState,
  restartLocalProxy,
  generateLocalSslCertificate
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
import ADL.Config(ToolConfig(..), DeployContextFile(..), DeployMode(..), ProxyModeConfig(..), SslCertMode(..),  SslCertPaths(..))
import ADL.State(State(..), Deploy(..))
import ADL.Types(EndPointLabel, DeployLabel)
import Commands.ProxyMode.Types
import Util(unpackRelease,fetchDeployContext)
import Control.Monad(when)
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
import Types(IOR, REnv(..), getToolConfig, scopeInfo, info)

localState :: StateAccess
localState = StateAccess {
  sa_get=getState,
  sa_getSlaves=return [],
  sa_update=updateState
}

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
  pm <- getProxyModeConfig
  tcfg <- getToolConfig
  let newState = newStateFn state
  mapM_ (runAction stateFile) (stateUpdateActions (SM.toMap (pm_endPoints pm)) state newState)
  where
    runAction stateFile action = do
      state <- getState
      executeAction action
      liftIO $ adlToJsonFile stateFile (nextState action state)


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
    oldEndPoints = (catMaybes . map (getEndpointDeploy endPointMap newState) . SM.toList) (s_connections oldState)
    newEndPoints = (catMaybes . map (getEndpointDeploy endPointMap newState) . SM.toList) (s_connections newState)
    deploysToCreate = M.elems (M.difference newDeploys oldDeploys)
    deploysToUpdate = filter (\(d1,d2) -> d1 /= d2) (M.elems (M.intersectionWith (\d1 d2 -> (d1,d2)) oldDeploys newDeploys))
    deploysToDestroy = M.elems (M.difference oldDeploys newDeploys)
    updateDeploy (d1,d2) = [DestroyDeploy d1, CreateDeploy d2]

getEndpointDeploy :: M.Map EndPointLabel EndPoint -> State -> (EndPointLabel,DeployLabel) -> Maybe (EndPoint,Deploy)
getEndpointDeploy endPointMap state (eplabel, dlabel) = do
  endPoint <- find (\ep -> ep_label ep == eplabel)  endPointMap
  deploy <- find (\d -> d_label d == dlabel) (SM.toMap (s_deploys state))
  return (endPoint,deploy)

-- | Execute the effects of a single action action
executeAction :: StateAction -> IOR ()

executeAction (CreateDeploy d) = do
  scopeInfo "execute CreateDeploy" $ do
    tcfg <- getToolConfig
    pm <- getProxyModeConfig
    fetchDeployContext Nothing
    let deployDir = T.unpack (tc_releasesDir tcfg) </> (takeBaseName (T.unpack (d_release d)))
    liftIO $ createDirectoryIfMissing True deployDir
    unpackRelease (contextWithLocalPorts pm (d_port d)) (d_release d) deployDir

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

executeAction (SetEndPoints liveEndPoints) = do
  scopeInfo "execute SetEndPoints" $ do
    tcfg <- getToolConfig
    allEndPoints <- fmap pm_endPoints getProxyModeConfig
    proxyDir <- getProxyDir
    scopeInfo "writing proxy config files" $ liftIO $ do
      writeProxyDockerCompose tcfg (proxyDir </> "docker-compose.yml")
      writeNginxConfig tcfg (proxyDir </> "nginx.conf") (maybeEndpoints allEndPoints liveEndPoints)
    callCommandInDir proxyDir "docker-compose up -d"
    callCommandInDir proxyDir "docker kill --signal=SIGHUP frontendproxy"
    where
      maybeEndpoints :: SM.StringMap EndPoint -> [(EndPoint,Deploy)] -> [(EndPoint,Maybe Deploy)]
      maybeEndpoints eps liveEndPoints = [ (ep,findDeploy label) | (label,ep) <- SM.toList eps]
        where
          findDeploy label = fmap snd (find ( (==label) . ep_label . fst) liveEndPoints)

getState :: IOR State
getState = do
  stateFile <- getStateFile
  exists <- liftIO $ doesFileExist stateFile
  case exists of
    True  -> liftIO $ adlFromJsonFile' stateFile
    False -> return emptyState

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

restartLocalProxy :: IOR ()
restartLocalProxy = do
  scopeInfo "restarting local proxy" $ do
    state <- getState
    pm <- getProxyModeConfig
    let endPoints = (catMaybes . map (getEndpointDeploy endPointMap state) . SM.toList) (s_connections state)
        endPointMap = SM.toMap (pm_endPoints pm)
    executeAction (SetEndPoints endPoints)

--- If any endpoints are configured for an autogenerated certificate,
--- generate a single certificate that covers them all. Use certbot
--- running under docker, in webroot mode such that it can answer the
--- challenges with the running local proxy.
generateLocalSslCertificate :: IOR ()
generateLocalSslCertificate = do
  scopeInfo "generating certificate with letsencrypt" $ do
    restartLocalProxy
    tcfg <- getToolConfig
    pm <- getProxyModeConfig
    proxyDir <- getProxyDir
    let serverNames = concat [serverNames |
          EndPoint{ep_serverNames=serverNames,ep_etype=Ep_httpsWithRedirect Scm_generated} <- M.elems (SM.toMap (pm_endPoints pm))]
        ledir = tc_letsencryptPrefixDir tcfg
        lewwwdir = tc_letsencryptWwwDir tcfg
        certbotCmd = T.intercalate " " (
          [ "docker run --rm --name " <> tc_autoCertName tcfg
          , "-v \"" <> ledir <> "/etc/letsencrypt:/etc/letsencrypt\""
          , "-v \"" <> ledir <> "/var/log/letsencrypt:/var/log/letsencrypt\""
          , "-v \"" <> lewwwdir <> "/.well-known/acme-challenge:" <> lewwwdir <> "/.well-known/acme-challenge\""
          , "certbot/certbot certonly"
          , "--cert-name " <> tc_autoCertName tcfg
          , "--preferred-challenge http-01"
          , "--webroot --webroot-path " <> lewwwdir
          , "-m " <> tc_autoCertContactEmail tcfg
          , "--deploy-hook 'chmod -R ag+rX /etc/letsencrypt'"
          , "--non-interactive"
          , "--agree-tos"
          , "--debug"
          ] <>
          [ "-d " <> serverName | serverName <- serverNames]
          )

    when (tc_autoCertContactEmail tcfg == "") $ do
      error "Need autoCertContactEmail specified in the config"

    case serverNames of
      [] -> info "No servers requiring a certificate"
      _ -> do
        callCommandInDir proxyDir certbotCmd

writeProxyDockerCompose :: ToolConfig -> FilePath -> IO ()
writeProxyDockerCompose tcfg path = T.writeFile path (T.intercalate "\n" lines)
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
      , "      - " <> ledir <> "/etc/letsencrypt:" <> ledir <> "/etc/letsencrypt"
      , "      - " <> lewwwdir <> ":" <> lewwwdir
      ]
    ledir = tc_letsencryptPrefixDir tcfg
    lewwwdir = tc_letsencryptWwwDir tcfg

writeNginxConfig :: ToolConfig -> FilePath -> [(EndPoint,Maybe Deploy)] -> IO ()
writeNginxConfig tcfg path eps = T.writeFile path (T.intercalate "\n" lines)
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
      , "  server_names_hash_bucket_size " <> serverNamesHashBucketSize <> ";"
      , ""
      , "  keepalive_timeout  65;"
      , "  client_max_body_size 0;"
      , ""
      , "  proxy_buffering on;"
      , "  proxy_temp_path proxy_temp 1 2;"
      , "  proxy_http_version 1.1;"
      , ""
      , "  charset utf-8;"
      , ""
      , "  # Run a default server just to keep ALB health checks happy"
      , "  server {"
      , "    listen 80 default_server;"
      , "    location /health-check {"
      , "       return 200;"
      , "    }"
      , "  }"
      , ""
      ] <>
      concat (map serverBlock eps) <>
      [ "}"
      ]

    -- The default of 64 or 32 is doesn't seem to be enought to handle long host names
    serverNamesHashBucketSize = "128"

    serverBlock (ep@EndPoint{ep_etype=Ep_httpOnly},Just d) =
      [ "  server {"
      , "    listen 80;"
      , "    server_name " <> T.intercalate " " (ep_serverNames ep) <> ";"
      , "    location / {"
      , "      proxy_set_header Host $host;"
      , "      proxy_pass http://localhost:" <> showText (d_port d) <> "/;"
      , "    }"
      , "  }"
      ]
    serverBlock (ep@EndPoint{ep_etype=Ep_httpOnly},Nothing) =
      [ "  server {"
      , "    listen 80;"
      , "    server_name " <> T.intercalate " " (ep_serverNames ep) <> ";"
      , "    return 503;"
      , "  }"
      ]
    serverBlock (ep@EndPoint{ep_etype=Ep_httpsWithRedirect certMode},Just d) =
      [ "  server {"
      , "    listen 80;"
      , "    server_name " <> T.intercalate " " (ep_serverNames ep) <> ";"
      , "    location '/.well-known/acme-challenge' {"
      , "        default_type \"text/plain\";"
      , "        alias " <> tc_letsencryptWwwDir tcfg <> "/.well-known/acme-challenge;"
      , "    }"
      , "    location / {"
      , "      return 301 https://$server_name$request_uri;"
      , "    }"
      , "  }"
      , "  server {"
      , "    listen       443 ssl;"
      , "    server_name " <> T.intercalate " " (ep_serverNames ep) <> ";"
      , "    ssl_certificate " <> sslCertPath certMode <> ";"
      , "    ssl_certificate_key " <> sslCertKeyPath certMode <> ";"
      , "    location / {"
      , "      proxy_set_header Host $host;"
      , "      proxy_pass http://localhost:" <> showText (d_port d) <> "/;"
      , "    }"
      , "  }"
      ]
    serverBlock (ep@EndPoint{ep_etype=Ep_httpsWithRedirect certMode},Nothing) =
      [ "  server {"
      , "    listen 80;"
      , "    server_name " <> T.intercalate " " (ep_serverNames ep) <> ";"
      , "    location '/.well-known/acme-challenge' {"
      , "        default_type \"text/plain\";"
      , "        alias " <> tc_letsencryptWwwDir tcfg <> "/.well-known/acme-challenge;"
      , "    }"
      , "    location / {"
      , "      return 503;"
      , "    }"
      , "  }"
      ]

    sslCertPath :: SslCertMode -> T.Text
    sslCertPath (Scm_explicit scp) = scp_sslCertificate scp
    sslCertPath Scm_generated = tc_letsencryptPrefixDir tcfg <> "/etc/letsencrypt/live/" <> (tc_autoCertName tcfg) <> "/fullchain.pem"

    sslCertKeyPath :: SslCertMode -> T.Text
    sslCertKeyPath (Scm_explicit scp) = scp_sslCertificateKey scp
    sslCertKeyPath Scm_generated = tc_letsencryptPrefixDir tcfg <> "/etc/letsencrypt/live/" <> (tc_autoCertName tcfg) <>"/privkey.pem";

-- Extend the release template context with port variables,
-- which include the http port where the deploy will make
-- itself available, and some extra ones for general purpose
-- ad-hoc use in the deploy.

contextWithLocalPorts :: ProxyModeConfig -> Word32 -> JS.Value -> JS.Value
contextWithLocalPorts pm port (JS.Object o) = JS.Object (HM.insert "ports" (JS.toJSON ports) o)
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
    (minPort,maxPort) = pm_dynamicPortRange pm
contextWithLocalPorts _ _ ctx = ctx

callCommandInDir :: FilePath -> T.Text -> IOR ()
callCommandInDir inDir cmd = do
  scopeInfo ("Running: " <> cmd)  $ do
    liftIO $ withCurrentDirectory inDir $ callCommand (T.unpack cmd)

getReleaseConfig :: FilePath -> IOR ReleaseConfig
getReleaseConfig deployDir = do
  liftIO $ adlFromJsonFile' (deployDir </> "release.json")
