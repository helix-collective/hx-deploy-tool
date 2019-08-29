module Commands.ProxyMode.Types where

import qualified ADL.Core.StringMap as SM
import qualified Data.Text as T

import ADL.Config(EndPoint(..), ToolConfig(..), DeployMode(..), ProxyModeConfig(..))
import ADL.Types(EndPointLabel)
import ADL.State(State(..), Deploy(..), SlaveState(..))
import Types(IOR, REnv(..), getToolConfig, scopeInfo)
import Data.Time(UTCTime)

data StateAccess = StateAccess {
  sa_get :: IOR State,
  sa_getSlaves :: IOR [(T.Text,LastModified SlaveState)],
  sa_update :: (State -> State) -> IOR ()
}

data LastModified a = LastModified {
  lm_value:: a,
  lm_modifiedAt:: Maybe UTCTime
}

type LabelledEndpoint = (EndPointLabel,EndPoint)

-- | The actions we can apply to change the state
data StateAction
  = CreateDeploy Deploy
  | DestroyDeploy Deploy
  | SetEndPoints [(LabelledEndpoint,Deploy)]

-- | Extract the proxy mode configuration from the environment
getProxyModeConfig :: IOR ProxyModeConfig
getProxyModeConfig = do
  tcfg <- getToolConfig
  case (tc_deployMode tcfg) of
    (DeployMode_proxy pm) -> return pm
    _ -> error "Proxy not enabled in configuration"

pmEndPoints :: ProxyModeConfig -> [LabelledEndpoint]
pmEndPoints pm = SM.toList(pm_endPoints pm)

-- | Update the state with the effect of an action
nextState :: StateAction -> State -> State
nextState (CreateDeploy d) s = s{s_deploys=SM.insert (d_label d) d (s_deploys s)}
nextState (DestroyDeploy d) s = s{s_deploys= SM.delete (d_label d) (s_deploys s)}
nextState (SetEndPoints eps) s = s{s_connections=SM.fromList [ (epl, d_label d) | ((epl,ep),d) <- eps ]}

showText :: Show a => a -> T.Text
showText = T.pack . show


-- | The default empty state
emptyState = State mempty mempty
