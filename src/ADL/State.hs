{-# LANGUAGE OverloadedStrings #-}
module ADL.State(
    Deploy(..),
    SlaveState(..),
    SlaveStatus(..),
    State(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

data Deploy = Deploy
    { d_label :: ADL.Types.DeployLabel
    , d_release :: T.Text
    , d_port :: Data.Word.Word32
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkDeploy :: ADL.Types.DeployLabel -> T.Text -> Data.Word.Word32 -> Deploy
mkDeploy label release port = Deploy label release port

instance AdlValue Deploy where
    atype _ = "state.Deploy"
    
    jsonGen = genObject
        [ genField "label" d_label
        , genField "release" d_release
        , genField "port" d_port
        ]
    
    jsonParser = Deploy
        <$> parseField "label"
        <*> parseField "release"
        <*> parseField "port"

data SlaveState = SlaveState
    { slaveState_status :: SlaveStatus
    , slaveState_slaveIP :: T.Text
    , slaveState_slaveHostName :: T.Text
    , slaveState_state :: State
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkSlaveState :: SlaveStatus -> T.Text -> T.Text -> State -> SlaveState
mkSlaveState status slaveIP slaveHostName state = SlaveState status slaveIP slaveHostName state

instance AdlValue SlaveState where
    atype _ = "state.SlaveState"
    
    jsonGen = genObject
        [ genField "status" slaveState_status
        , genField "slaveIP" slaveState_slaveIP
        , genField "slaveHostName" slaveState_slaveHostName
        , genField "state" slaveState_state
        ]
    
    jsonParser = SlaveState
        <$> parseField "status"
        <*> parseField "slaveIP"
        <*> parseField "slaveHostName"
        <*> parseField "state"

data SlaveStatus
    = SlaveStatus_ok
    | SlaveStatus_error T.Text
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue SlaveStatus where
    atype _ = "state.SlaveStatus"
    
    jsonGen = genUnion (\jv -> case jv of
        SlaveStatus_ok -> genUnionVoid "ok"
        SlaveStatus_error v -> genUnionValue "error" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "ok" -> parseUnionVoid SlaveStatus_ok
        "error" ->  parseUnionValue SlaveStatus_error
        _ -> parseFail "expected a discriminator for SlaveStatus (ok,error)" 

data State = State
    { s_deploys :: (ADL.Types.StringKeyMap ADL.Types.DeployLabel Deploy)
    , s_connections :: (ADL.Types.StringKeyMap ADL.Types.EndPointLabel ADL.Types.DeployLabel)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkState :: (ADL.Types.StringKeyMap ADL.Types.DeployLabel Deploy) -> (ADL.Types.StringKeyMap ADL.Types.EndPointLabel ADL.Types.DeployLabel) -> State
mkState deploys connections = State deploys connections

instance AdlValue State where
    atype _ = "state.State"
    
    jsonGen = genObject
        [ genField "deploys" s_deploys
        , genField "connections" s_connections
        ]
    
    jsonParser = State
        <$> parseField "deploys"
        <*> parseField "connections"