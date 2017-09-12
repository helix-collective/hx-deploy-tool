{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    DeployContextFile(..),
    ToolConfig(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Prelude

data DeployContextFile = DeployContextFile
    { dcf_name :: ADL.Types.FilePath
    , dcf_source :: ADL.Types.S3Path
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkDeployContextFile :: ADL.Types.FilePath -> ADL.Types.S3Path -> DeployContextFile
mkDeployContextFile name source = DeployContextFile name source

instance AdlValue DeployContextFile where
    atype _ = "config.DeployContextFile"
    
    jsonGen = genObject
        [ genField "name" dcf_name
        , genField "source" dcf_source
        ]
    
    jsonParser = DeployContextFile
        <$> parseField "name"
        <*> parseField "source"

data ToolConfig = ToolConfig
    { tc_releasesDir :: ADL.Types.FilePath
    , tc_contextCache :: ADL.Types.FilePath
    , tc_releasesS3 :: ADL.Types.S3Path
    , tc_deployContextFiles :: [DeployContextFile]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkToolConfig :: ADL.Types.S3Path -> [DeployContextFile] -> ToolConfig
mkToolConfig releasesS3 deployContextFiles = ToolConfig "/opt/releases" "/opt/etc/deployment" releasesS3 deployContextFiles

instance AdlValue ToolConfig where
    atype _ = "config.ToolConfig"
    
    jsonGen = genObject
        [ genField "releasesDir" tc_releasesDir
        , genField "contextCache" tc_contextCache
        , genField "releasesS3" tc_releasesS3
        , genField "deployContextFiles" tc_deployContextFiles
        ]
    
    jsonParser = ToolConfig
        <$> parseFieldDef "releasesDir" "/opt/releases"
        <*> parseFieldDef "contextCache" "/opt/etc/deployment"
        <*> parseField "releasesS3"
        <*> parseField "deployContextFiles"