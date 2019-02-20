module Commands.ProxyMode.Types where

import qualified ADL.Core.StringMap as SM
import qualified Data.Text as T

import ADL.Config(EndPoint(..), ToolConfig(..), DeployMode(..), ProxyModeConfig(..))
import ADL.State(State(..), Deploy(..))
import Types(IOR, REnv(..), getToolConfig, scopeInfo)

data StateAccess = StateAccess {
  sa_get :: IOR State,
  sa_getSlaves :: IOR [(T.Text,State)],
  sa_update :: (State -> State) -> IOR ()
}

-- | The actions we can apply to change the state
data StateAction
  = CreateDeploy Deploy
  | DestroyDeploy Deploy
  | SetEndPoints [(EndPoint,Deploy)]

-- | Extract the proxy mode configuration from the environment
getProxyModeConfig :: IOR ProxyModeConfig
getProxyModeConfig = do
  tcfg <- getToolConfig
  case (tc_deployMode tcfg) of
    (DeployMode_proxy pm) -> return pm
    _ -> error "Proxy not enabled in configuration"

pmEndPoints :: ProxyModeConfig -> [EndPoint]
pmEndPoints pm = SM.elems (pm_endPoints pm)

-- | Update the state with the effect of an action
nextState :: StateAction -> State -> State
nextState (CreateDeploy d) s = s{s_deploys=SM.insert (d_label d) d (s_deploys s)}
nextState (DestroyDeploy d) s = s{s_deploys= SM.delete (d_label d) (s_deploys s)}
nextState (SetEndPoints eps) s = s{s_connections=SM.fromList [ (ep_label ep, d_label d) | (ep,d) <- eps ]}

showText :: Show a => a -> T.Text
showText = T.pack . show


-- | The default empty state
emptyState = State mempty mempty
