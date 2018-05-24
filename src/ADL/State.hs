{-# LANGUAGE OverloadedStrings #-}
module ADL.State(
    Deploy(..),
    State(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
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

data State = State
    { s_deploys :: StringMap (Deploy)
    , s_connections :: StringMap (ADL.Types.DeployLabel)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkState :: StringMap (Deploy) -> StringMap (ADL.Types.DeployLabel) -> State
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