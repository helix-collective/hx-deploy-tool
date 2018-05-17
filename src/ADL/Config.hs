{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    DeployContextFile(..),
    LetsEncryptConfig(..),
    ToolConfig(..),
    Verbosity(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
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

data LetsEncryptConfig = LetsEncryptConfig
    { lec_certbotPath :: T.Text
    , lec_awsHostedZoneId :: T.Text
    , lec_basedir :: T.Text
    , lec_email :: T.Text
    , lec_domains :: [T.Text]
    , lec_verbosity :: Verbosity
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkLetsEncryptConfig :: T.Text -> T.Text -> T.Text -> T.Text -> [T.Text] -> LetsEncryptConfig
mkLetsEncryptConfig certbotPath awsHostedZoneId basedir email domains = LetsEncryptConfig certbotPath awsHostedZoneId basedir email domains Verbosity_quiet

instance AdlValue LetsEncryptConfig where
    atype _ = "config.LetsEncryptConfig"
    
    jsonGen = genObject
        [ genField "certbotPath" lec_certbotPath
        , genField "awsHostedZoneId" lec_awsHostedZoneId
        , genField "basedir" lec_basedir
        , genField "email" lec_email
        , genField "domains" lec_domains
        , genField "verbosity" lec_verbosity
        ]
    
    jsonParser = LetsEncryptConfig
        <$> parseField "certbotPath"
        <*> parseField "awsHostedZoneId"
        <*> parseField "basedir"
        <*> parseField "email"
        <*> parseField "domains"
        <*> parseFieldDef "verbosity" Verbosity_quiet

data ToolConfig = ToolConfig
    { tc_releasesDir :: ADL.Types.FilePath
    , tc_contextCache :: ADL.Types.FilePath
    , tc_logFile :: ADL.Types.FilePath
    , tc_releasesS3 :: ADL.Types.S3Path
    , tc_deployContextFiles :: [DeployContextFile]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkToolConfig :: ADL.Types.S3Path -> [DeployContextFile] -> ToolConfig
mkToolConfig releasesS3 deployContextFiles = ToolConfig "/opt/releases" "/opt/etc/deployment" "/opt/var/log/hx-deploy-tool.log" releasesS3 deployContextFiles

instance AdlValue ToolConfig where
    atype _ = "config.ToolConfig"
    
    jsonGen = genObject
        [ genField "releasesDir" tc_releasesDir
        , genField "contextCache" tc_contextCache
        , genField "logFile" tc_logFile
        , genField "releasesS3" tc_releasesS3
        , genField "deployContextFiles" tc_deployContextFiles
        ]
    
    jsonParser = ToolConfig
        <$> parseFieldDef "releasesDir" "/opt/releases"
        <*> parseFieldDef "contextCache" "/opt/etc/deployment"
        <*> parseFieldDef "logFile" "/opt/var/log/hx-deploy-tool.log"
        <*> parseField "releasesS3"
        <*> parseField "deployContextFiles"

data Verbosity
    = Verbosity_quiet
    | Verbosity_noisy
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue Verbosity where
    atype _ = "config.Verbosity"
    
    jsonGen = genUnion (\jv -> case jv of
        Verbosity_quiet -> genUnionVoid "quiet"
        Verbosity_noisy -> genUnionVoid "noisy"
        )
    
    jsonParser
        =   parseUnionVoid "quiet" Verbosity_quiet
        <|> parseUnionVoid "noisy" Verbosity_noisy
        <|> parseFail "expected a Verbosity"