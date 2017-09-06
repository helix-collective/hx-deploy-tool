{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    DeployContextFile(..),
    FilePath,
    S3Path,
    ToolConfig(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data DeployContextFile = DeployContextFile
    { deployContextFile_name :: FilePath
    , deployContextFile_source :: S3Path
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkDeployContextFile :: FilePath -> S3Path -> DeployContextFile
mkDeployContextFile name source = DeployContextFile name source

instance AdlValue DeployContextFile where
    atype _ = "config.DeployContextFile"
    
    jsonGen = genObject
        [ genField "name" deployContextFile_name
        , genField "source" deployContextFile_source
        ]
    
    jsonParser = DeployContextFile
        <$> parseField "name"
        <*> parseField "source"

type FilePath = T.Text

type S3Path = T.Text

data ToolConfig = ToolConfig
    { toolConfig_releasesDir :: FilePath
    , toolConfig_contextCache :: FilePath
    , toolConfig_deployContextFiles :: [DeployContextFile]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkToolConfig :: [DeployContextFile] -> ToolConfig
mkToolConfig deployContextFiles = ToolConfig "/opt/releases" "/opt/etc/deployment" deployContextFiles

instance AdlValue ToolConfig where
    atype _ = "config.ToolConfig"
    
    jsonGen = genObject
        [ genField "releasesDir" toolConfig_releasesDir
        , genField "contextCache" toolConfig_contextCache
        , genField "deployContextFiles" toolConfig_deployContextFiles
        ]
    
    jsonParser = ToolConfig
        <$> parseFieldDef "releasesDir" "/opt/releases"
        <*> parseFieldDef "contextCache" "/opt/etc/deployment"
        <*> parseField "deployContextFiles"