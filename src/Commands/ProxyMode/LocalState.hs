{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Commands.ProxyMode.LocalState(
  localState,
  restartLocalProxy,
  generateLocalSslCertificate,
  nginxConfTemplate
) where

import qualified ADL.Core.StringMap as SM
import qualified ADL.Core.Nullable as NL
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Set as S
import qualified Text.Mustache as TM
import qualified Text.Mustache.Types as TM

import ADL.Config(EndPoint(..), EndPointType(..))
import ADL.Core(adlFromJsonFile', adlToJsonFile, adlToJson)
import ADL.Release(ReleaseConfig(..))
import ADL.Config(ToolConfig(..), DeployMode(..), ProxyModeConfig(..), SslCertMode(..),  SslCertPaths(..), HealthCheckConfig(..))
import ADL.Nginx(NginxConfContext(..), NginxHealthCheck(..), NginxEndPoint(..), NginxHttpEndPoint(..), NginxHttpsEndPoint(..))
import ADL.State(State(..), Deploy(..))
import ADL.Types(EndPointLabel, DeployLabel)
import Commands.ProxyMode.Types
import Util(unpackRelease,fetchConfigContext,removeNullKeys)
import Control.Monad(when)
import Control.Monad.Reader(ask)
import Control.Monad.IO.Class
import Data.FileEmbed(embedFile)
import Data.List(find, partition,elem)
import Data.Maybe(catMaybes)
import Data.Foldable(for_)
import Data.Monoid
import Data.Word
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory, removeDirectoryRecursive)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, replaceExtension, (</>))
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
stateUpdateActions endPointMap oldState newState =
  -- First shut down deploys that occupy ports that we need for new deploys
     map DestroyDeploy deploysToDestroyFirst

  -- create all new endpoints
  <> map CreateDeploy deploysToCreate

  -- update any deploys that have changed
  <> concatMap updateDeploy deploysToUpdate

  -- Setup the endpoints (ie the nginx config?) if any have changed
  <> if newEndPoints /= oldEndPoints then [SetEndPoints newEndPoints] else []

  -- Finally, delete the remainind endpoints that are no longer required
  <> map DestroyDeploy deploysToDestroyLast
  where
    oldDeploys = SM.toMap (s_deploys oldState)
    newDeploys = SM.toMap (s_deploys newState)
    oldEndPoints = (catMaybes . map (getEndpointDeploy endPointMap newState) . SM.toList) (s_connections oldState)
    newEndPoints = (catMaybes . map (getEndpointDeploy endPointMap newState) . SM.toList) (s_connections newState)
    deploysToCreate = M.elems (M.difference newDeploys oldDeploys)
    deploysToUpdate = filter (\(d1,d2) -> d1 /= d2) (M.elems (M.intersectionWith (\d1 d2 -> (d1,d2)) oldDeploys newDeploys))
    (deploysToDestroyFirst,deploysToDestroyLast) = partition usesRequiredPort (M.elems (M.difference oldDeploys newDeploys))
    updateDeploy (d1,d2) = [DestroyDeploy d1, CreateDeploy d2]
    usesRequiredPort d = elem (d_port d) requiredPorts
    requiredPorts =map d_port (M.elems newDeploys)

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
    fetchConfigContext Nothing
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
    pm <- getProxyModeConfig
    let allEndPoints = pm_endPoints pm
    proxyDir <- getProxyDir
    scopeInfo "writing proxy config files" $ liftIO $ do
      writeProxyDockerCompose tcfg (proxyDir </> "docker-compose.yml")
      writeNginxConfig tcfg pm (proxyDir </> "nginx.conf") (maybeEndpoints allEndPoints liveEndPoints)
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
      , "    restart: always"
      ]
    ledir = tc_letsencryptPrefixDir tcfg
    lewwwdir = tc_letsencryptWwwDir tcfg

writeNginxConfig :: ToolConfig -> ProxyModeConfig -> FilePath -> [(EndPoint,Maybe Deploy)] -> IO ()
writeNginxConfig tcfg pm path eps = do
  etemplate <- case pm_nginxConfTemplatePath pm of
    Nothing -> do
      return (TM.compileTemplate "nginx.conf.tpl" nginxConfTemplate)
    (Just templatePath0) -> do
      let templatePath = T.unpack templatePath0
      TM.automaticCompile [takeDirectory templatePath] templatePath

  context <- nginxConfigContext tcfg pm eps

  case etemplate of
   Left err -> error (show err)
   Right template -> do
     let json = removeNullKeys (adlToJson context)
         text = TM.substitute template json
     LBS.writeFile (replaceExtension path ".ctx.json") (JS.encode json)
     T.writeFile path text

-- build up the context to be injected into the nginx config template
nginxConfigContext:: ToolConfig -> ProxyModeConfig -> [(EndPoint,Maybe Deploy)] -> IO NginxConfContext
nginxConfigContext tcfg pm eps = do
  -- We only include the ssl paths for the generated certificate in
  -- the context if the physical paths exist on disk
  genSslCertPath <- existingFilePath (tc_letsencryptPrefixDir tcfg <> "/etc/letsencrypt/live/" <> (tc_autoCertName tcfg) <> "/fullchain.pem")
  genSslCertKeyPath <- existingFilePath (tc_letsencryptPrefixDir tcfg <> "/etc/letsencrypt/live/" <> (tc_autoCertName tcfg) <>"/privkey.pem")
  let context = NginxConfContext
        { ncc_healthCheck = case (tc_healthCheck tcfg, eps) of
            (Just hc,(_,Just deploy):_) -> NL.fromValue (NginxHealthCheck
               { nhc_incomingPath = hc_incomingPath hc
               , nhc_outgoingPath = hc_outgoingPath hc
               , nhc_outgoingPort = d_port deploy
               })
            _ -> NL.null
        , ncc_endPoints = fmap contextEndPoint eps
        }
      contextEndPoint (ep@EndPoint{ep_etype=Ep_httpOnly},deploy) = Ne_http
        NginxHttpEndPoint
        { nhe_serverNames = T.intercalate " " (ep_serverNames ep)
        , nhe_port = NL.fromMaybe (fmap d_port deploy)
        }
      contextEndPoint (ep@EndPoint{ep_etype=Ep_httpsWithRedirect certMode},deploy) = Ne_https
        NginxHttpsEndPoint
        { nhse_serverNames = T.intercalate " " (ep_serverNames ep)
        , nhse_port = NL.fromMaybe (fmap d_port deploy)
        , nhse_sslCertPath = sslCertPath certMode
        , nhse_sslCertKeyPath = sslCertKeyPath certMode
        , nhse_letsencryptWwwDir = tc_letsencryptWwwDir tcfg
        }
      sslCertPath :: SslCertMode -> NL.Nullable T.Text
      sslCertPath (Scm_explicit scp) = NL.fromValue (scp_sslCertificate scp)
      sslCertPath Scm_generated = genSslCertPath

      sslCertKeyPath :: SslCertMode -> NL.Nullable T.Text
      sslCertKeyPath (Scm_explicit scp) = NL.fromValue (scp_sslCertificateKey scp)
      sslCertKeyPath Scm_generated = genSslCertKeyPath

  return context

existingFilePath :: T.Text -> IO (NL.Nullable T.Text)
existingFilePath path = do
  exists <- doesFileExist (T.unpack path)
  return (if exists then (NL.fromValue path) else NL.null)

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


nginxConfTemplate :: T.Text
nginxConfTemplate = T.decodeUtf8 $(embedFile "config/nginx.conf.tpl")
