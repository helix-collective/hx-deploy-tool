{-# LANGUAGE OverloadedStrings #-}
module ADL.Config(
    BlobStoreConfig(..),
    DeployContextFile(..),
    DeployMode(..),
    EndPoint(..),
    EndPointType(..),
    LetsEncryptConfig(..),
    MachineLabel(..),
    ProxyModeConfig(..),
    SslCertMode(..),
    SslCertPaths(..),
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

data BlobStoreConfig
    = BlobStoreConfig_s3 ADL.Types.S3Path
    | BlobStoreConfig_localdir ADL.Types.FilePath
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue BlobStoreConfig where
    atype _ = "config.BlobStoreConfig"
    
    jsonGen = genUnion (\jv -> case jv of
        BlobStoreConfig_s3 v -> genUnionValue "s3" v
        BlobStoreConfig_localdir v -> genUnionValue "localdir" v
        )
    
    jsonParser
        =   parseUnionValue "s3" BlobStoreConfig_s3
        <|> parseUnionValue "localdir" BlobStoreConfig_localdir
        <|> parseFail "expected a BlobStoreConfig"

data DeployContextFile = DeployContextFile
    { dcf_name :: ADL.Types.FilePath
    , dcf_sourceName :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkDeployContextFile :: ADL.Types.FilePath -> T.Text -> DeployContextFile
mkDeployContextFile name sourceName = DeployContextFile name sourceName

instance AdlValue DeployContextFile where
    atype _ = "config.DeployContextFile"
    
    jsonGen = genObject
        [ genField "name" dcf_name
        , genField "sourceName" dcf_sourceName
        ]
    
    jsonParser = DeployContextFile
        <$> parseField "name"
        <*> parseField "sourceName"

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
    , ep_etype :: EndPointType
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkEndPoint :: ADL.Types.EndPointLabel -> T.Text -> EndPointType -> EndPoint
mkEndPoint label serverName etype = EndPoint label serverName etype

instance AdlValue EndPoint where
    atype _ = "config.EndPoint"
    
    jsonGen = genObject
        [ genField "label" ep_label
        , genField "serverName" ep_serverName
        , genField "etype" ep_etype
        ]
    
    jsonParser = EndPoint
        <$> parseField "label"
        <*> parseField "serverName"
        <*> parseField "etype"

data EndPointType
    = Ep_httpOnly
    | Ep_httpsWithRedirect SslCertMode
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue EndPointType where
    atype _ = "config.EndPointType"
    
    jsonGen = genUnion (\jv -> case jv of
        Ep_httpOnly -> genUnionVoid "httpOnly"
        Ep_httpsWithRedirect v -> genUnionValue "httpsWithRedirect" v
        )
    
    jsonParser
        =   parseUnionVoid "httpOnly" Ep_httpOnly
        <|> parseUnionValue "httpsWithRedirect" Ep_httpsWithRedirect
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

data MachineLabel
    = MachineLabel_label T.Text
    | MachineLabel_ec2InstanceId
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue MachineLabel where
    atype _ = "config.MachineLabel"
    
    jsonGen = genUnion (\jv -> case jv of
        MachineLabel_label v -> genUnionValue "label" v
        MachineLabel_ec2InstanceId -> genUnionVoid "ec2InstanceId"
        )
    
    jsonParser
        =   parseUnionValue "label" MachineLabel_label
        <|> parseUnionVoid "ec2InstanceId" MachineLabel_ec2InstanceId
        <|> parseFail "expected a MachineLabel"

data ProxyModeConfig = ProxyModeConfig
    { pm_endPoints :: StringMap (EndPoint)
    , pm_remoteStateS3 :: (ADL.Sys.Types.Maybe ADL.Types.S3Path)
    , pm_dynamicPortRange :: (ADL.Sys.Types.Pair Data.Word.Word32 Data.Word.Word32)
    , pm_slaveLabel :: MachineLabel
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkProxyModeConfig :: StringMap (EndPoint) -> ProxyModeConfig
mkProxyModeConfig endPoints = ProxyModeConfig endPoints Prelude.Nothing ((,) 8000 8100) MachineLabel_ec2InstanceId

instance AdlValue ProxyModeConfig where
    atype _ = "config.ProxyModeConfig"
    
    jsonGen = genObject
        [ genField "endPoints" pm_endPoints
        , genField "remoteStateS3" pm_remoteStateS3
        , genField "dynamicPortRange" pm_dynamicPortRange
        , genField "slaveLabel" pm_slaveLabel
        ]
    
    jsonParser = ProxyModeConfig
        <$> parseField "endPoints"
        <*> parseFieldDef "remoteStateS3" Prelude.Nothing
        <*> parseFieldDef "dynamicPortRange" ((,) 8000 8100)
        <*> parseFieldDef "slaveLabel" MachineLabel_ec2InstanceId

data SslCertMode
    = Scm_generated
    | Scm_explicit SslCertPaths
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue SslCertMode where
    atype _ = "config.SslCertMode"
    
    jsonGen = genUnion (\jv -> case jv of
        Scm_generated -> genUnionVoid "generated"
        Scm_explicit v -> genUnionValue "explicit" v
        )
    
    jsonParser
        =   parseUnionVoid "generated" Scm_generated
        <|> parseUnionValue "explicit" Scm_explicit
        <|> parseFail "expected a SslCertMode"

data SslCertPaths = SslCertPaths
    { scp_sslCertificate :: ADL.Types.FilePath
    , scp_sslCertificateKey :: ADL.Types.FilePath
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkSslCertPaths :: ADL.Types.FilePath -> ADL.Types.FilePath -> SslCertPaths
mkSslCertPaths sslCertificate sslCertificateKey = SslCertPaths sslCertificate sslCertificateKey

instance AdlValue SslCertPaths where
    atype _ = "config.SslCertPaths"
    
    jsonGen = genObject
        [ genField "sslCertificate" scp_sslCertificate
        , genField "sslCertificateKey" scp_sslCertificateKey
        ]
    
    jsonParser = SslCertPaths
        <$> parseField "sslCertificate"
        <*> parseField "sslCertificateKey"

data ToolConfig = ToolConfig
    { tc_releasesDir :: ADL.Types.FilePath
    , tc_contextCache :: ADL.Types.FilePath
    , tc_logFile :: ADL.Types.FilePath
    , tc_letsencryptPrefixDir :: ADL.Types.FilePath
    , tc_letsencryptWwwDir :: ADL.Types.FilePath
    , tc_autoCertName :: T.Text
    , tc_autoCertContactEmail :: T.Text
    , tc_releases :: BlobStoreConfig
    , tc_deployContext :: BlobStoreConfig
    , tc_deployContextFiles :: [DeployContextFile]
    , tc_deployMode :: DeployMode
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkToolConfig :: BlobStoreConfig -> BlobStoreConfig -> [DeployContextFile] -> ToolConfig
mkToolConfig releases deployContext deployContextFiles = ToolConfig "/opt/releases" "/opt/etc/deployment" "/opt/var/log/hx-deploy-tool.log" "/opt" "/opt/var/www" "hxdeploytoolcert" "" releases deployContext deployContextFiles DeployMode_select

instance AdlValue ToolConfig where
    atype _ = "config.ToolConfig"
    
    jsonGen = genObject
        [ genField "releasesDir" tc_releasesDir
        , genField "contextCache" tc_contextCache
        , genField "logFile" tc_logFile
        , genField "letsencryptPrefixDir" tc_letsencryptPrefixDir
        , genField "letsencryptWwwDir" tc_letsencryptWwwDir
        , genField "autoCertName" tc_autoCertName
        , genField "autoCertContactEmail" tc_autoCertContactEmail
        , genField "releases" tc_releases
        , genField "deployContext" tc_deployContext
        , genField "deployContextFiles" tc_deployContextFiles
        , genField "deployMode" tc_deployMode
        ]
    
    jsonParser = ToolConfig
        <$> parseFieldDef "releasesDir" "/opt/releases"
        <*> parseFieldDef "contextCache" "/opt/etc/deployment"
        <*> parseFieldDef "logFile" "/opt/var/log/hx-deploy-tool.log"
        <*> parseFieldDef "letsencryptPrefixDir" "/opt"
        <*> parseFieldDef "letsencryptWwwDir" "/opt/var/www"
        <*> parseFieldDef "autoCertName" "hxdeploytoolcert"
        <*> parseFieldDef "autoCertContactEmail" ""
        <*> parseField "releases"
        <*> parseField "deployContext"
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