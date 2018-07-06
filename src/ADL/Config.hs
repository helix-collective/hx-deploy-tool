{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    DeployContextFile(..),
    DeployMode(..),
    EndPoint(..),
    EndPointType(..),
    LetsEncryptConfig(..),
    ProxyModeConfig(..),
    ToolConfig(..),
    Verbosity(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified ADL.Sys.Types
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Data.Word
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

data DeployMode
    = DeployMode_select
    | DeployMode_proxy ProxyModeConfig
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue DeployMode where
    atype _ = "config.DeployMode"
    
    jsonGen = genUnion (\jv -> case jv of
        DeployMode_select -> genUnionVoid "select"
        DeployMode_proxy v -> genUnionValue "proxy" v
        )
    
    jsonParser
        =   parseUnionVoid "select" DeployMode_select
        <|> parseUnionValue "proxy" DeployMode_proxy
        <|> parseFail "expected a DeployMode"

data EndPoint = EndPoint
    { ep_label :: ADL.Types.EndPointLabel
    , ep_serverName :: T.Text
    , ep_sslCertDir :: T.Text
    , ep_etype :: EndPointType
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkEndPoint :: ADL.Types.EndPointLabel -> T.Text -> T.Text -> EndPointType -> EndPoint
mkEndPoint label serverName sslCertDir etype = EndPoint label serverName sslCertDir etype

instance AdlValue EndPoint where
    atype _ = "config.EndPoint"
    
    jsonGen = genObject
        [ genField "label" ep_label
        , genField "serverName" ep_serverName
        , genField "sslCertDir" ep_sslCertDir
        , genField "etype" ep_etype
        ]
    
    jsonParser = EndPoint
        <$> parseField "label"
        <*> parseField "serverName"
        <*> parseField "sslCertDir"
        <*> parseField "etype"

data EndPointType
    = Ep_httpOnly
    | Ep_httpsWithRedirect
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue EndPointType where
    atype _ = "config.EndPointType"
    
    jsonGen = genUnion (\jv -> case jv of
        Ep_httpOnly -> genUnionVoid "httpOnly"
        Ep_httpsWithRedirect -> genUnionVoid "httpsWithRedirect"
        )
    
    jsonParser
        =   parseUnionVoid "httpOnly" Ep_httpOnly
        <|> parseUnionVoid "httpsWithRedirect" Ep_httpsWithRedirect
        <|> parseFail "expected a EndPointType"

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

data ProxyModeConfig = ProxyModeConfig
    { pm_endPoints :: StringMap (EndPoint)
    , pm_remoteStateS3 :: (ADL.Sys.Types.Maybe ADL.Types.S3Path)
    , pm_dynamicPortRange :: (ADL.Sys.Types.Pair Data.Word.Word32 Data.Word.Word32)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkProxyModeConfig :: StringMap (EndPoint) -> ProxyModeConfig
mkProxyModeConfig endPoints = ProxyModeConfig endPoints Prelude.Nothing ((,) 8000 8100)

instance AdlValue ProxyModeConfig where
    atype _ = "config.ProxyModeConfig"
    
    jsonGen = genObject
        [ genField "endPoints" pm_endPoints
        , genField "remoteStateS3" pm_remoteStateS3
        , genField "dynamicPortRange" pm_dynamicPortRange
        ]
    
    jsonParser = ProxyModeConfig
        <$> parseField "endPoints"
        <*> parseFieldDef "remoteStateS3" Prelude.Nothing
        <*> parseFieldDef "dynamicPortRange" ((,) 8000 8100)

data ToolConfig = ToolConfig
    { tc_releasesDir :: ADL.Types.FilePath
    , tc_contextCache :: ADL.Types.FilePath
    , tc_logFile :: ADL.Types.FilePath
    , tc_releasesS3 :: ADL.Types.S3Path
    , tc_deployContextFiles :: [DeployContextFile]
    , tc_deployMode :: DeployMode
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkToolConfig :: ADL.Types.S3Path -> [DeployContextFile] -> ToolConfig
mkToolConfig releasesS3 deployContextFiles = ToolConfig "/opt/releases" "/opt/etc/deployment" "/opt/var/log/hx-deploy-tool.log" releasesS3 deployContextFiles DeployMode_select

instance AdlValue ToolConfig where
    atype _ = "config.ToolConfig"
    
    jsonGen = genObject
        [ genField "releasesDir" tc_releasesDir
        , genField "contextCache" tc_contextCache
        , genField "logFile" tc_logFile
        , genField "releasesS3" tc_releasesS3
        , genField "deployContextFiles" tc_deployContextFiles
        , genField "deployMode" tc_deployMode
        ]
    
    jsonParser = ToolConfig
        <$> parseFieldDef "releasesDir" "/opt/releases"
        <*> parseFieldDef "contextCache" "/opt/etc/deployment"
        <*> parseFieldDef "logFile" "/opt/var/log/hx-deploy-tool.log"
        <*> parseField "releasesS3"
        <*> parseField "deployContextFiles"
        <*> parseFieldDef "deployMode" DeployMode_select

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