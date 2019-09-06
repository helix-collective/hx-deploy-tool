{-# LANGUAGE OverloadedStrings #-}
module ADL.Nginx(
    NginxConfContext(..),
    NginxEndPoint(..),
    NginxHealthCheck(..),
    NginxHttpEndPoint(..),
    NginxHttpsEndPoint(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified ADL.Core.Nullable
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Data.Word
import qualified Prelude

data NginxConfContext = NginxConfContext
    { ncc_healthCheck :: ADL.Core.Nullable.Nullable (NginxHealthCheck)
    , ncc_endPoints :: [NginxEndPoint]
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkNginxConfContext :: ADL.Core.Nullable.Nullable (NginxHealthCheck) -> [NginxEndPoint] -> NginxConfContext
mkNginxConfContext healthCheck endPoints = NginxConfContext healthCheck endPoints

instance AdlValue NginxConfContext where
    atype _ = "nginx.NginxConfContext"
    
    jsonGen = genObject
        [ genField "healthCheck" ncc_healthCheck
        , genField "endPoints" ncc_endPoints
        ]
    
    jsonParser = NginxConfContext
        <$> parseField "healthCheck"
        <*> parseField "endPoints"

data NginxEndPoint
    = Ne_http NginxHttpEndPoint
    | Ne_https NginxHttpsEndPoint
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

instance AdlValue NginxEndPoint where
    atype _ = "nginx.NginxEndPoint"
    
    jsonGen = genUnion (\jv -> case jv of
        Ne_http v -> genUnionValue "http" v
        Ne_https v -> genUnionValue "https" v
        )
    
    jsonParser = parseUnion $ \disc -> case disc of
        "http" ->  parseUnionValue Ne_http
        "https" ->  parseUnionValue Ne_https
        _ -> parseFail "expected a discriminator for NginxEndPoint (http,https)" 

data NginxHealthCheck = NginxHealthCheck
    { nhc_incomingPath :: T.Text
    , nhc_outgoingPath :: T.Text
    , nhc_outgoingPort :: Data.Word.Word32
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkNginxHealthCheck :: T.Text -> T.Text -> Data.Word.Word32 -> NginxHealthCheck
mkNginxHealthCheck incomingPath outgoingPath outgoingPort = NginxHealthCheck incomingPath outgoingPath outgoingPort

instance AdlValue NginxHealthCheck where
    atype _ = "nginx.NginxHealthCheck"
    
    jsonGen = genObject
        [ genField "incomingPath" nhc_incomingPath
        , genField "outgoingPath" nhc_outgoingPath
        , genField "outgoingPort" nhc_outgoingPort
        ]
    
    jsonParser = NginxHealthCheck
        <$> parseField "incomingPath"
        <*> parseField "outgoingPath"
        <*> parseField "outgoingPort"

data NginxHttpEndPoint = NginxHttpEndPoint
    { nhe_serverNames :: T.Text
    , nhe_port :: ADL.Core.Nullable.Nullable (Data.Word.Word32)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkNginxHttpEndPoint :: T.Text -> ADL.Core.Nullable.Nullable (Data.Word.Word32) -> NginxHttpEndPoint
mkNginxHttpEndPoint serverNames port = NginxHttpEndPoint serverNames port

instance AdlValue NginxHttpEndPoint where
    atype _ = "nginx.NginxHttpEndPoint"
    
    jsonGen = genObject
        [ genField "serverNames" nhe_serverNames
        , genField "port" nhe_port
        ]
    
    jsonParser = NginxHttpEndPoint
        <$> parseField "serverNames"
        <*> parseField "port"

data NginxHttpsEndPoint = NginxHttpsEndPoint
    { nhse_serverNames :: T.Text
    , nhse_sslCertPath :: ADL.Core.Nullable.Nullable (T.Text)
    , nhse_sslCertKeyPath :: ADL.Core.Nullable.Nullable (T.Text)
    , nhse_letsencryptWwwDir :: T.Text
    , nhse_port :: ADL.Core.Nullable.Nullable (Data.Word.Word32)
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkNginxHttpsEndPoint :: T.Text -> ADL.Core.Nullable.Nullable (T.Text) -> ADL.Core.Nullable.Nullable (T.Text) -> T.Text -> ADL.Core.Nullable.Nullable (Data.Word.Word32) -> NginxHttpsEndPoint
mkNginxHttpsEndPoint serverNames sslCertPath sslCertKeyPath letsencryptWwwDir port = NginxHttpsEndPoint serverNames sslCertPath sslCertKeyPath letsencryptWwwDir port

instance AdlValue NginxHttpsEndPoint where
    atype _ = "nginx.NginxHttpsEndPoint"
    
    jsonGen = genObject
        [ genField "serverNames" nhse_serverNames
        , genField "sslCertPath" nhse_sslCertPath
        , genField "sslCertKeyPath" nhse_sslCertKeyPath
        , genField "letsencryptWwwDir" nhse_letsencryptWwwDir
        , genField "port" nhse_port
        ]
    
    jsonParser = NginxHttpsEndPoint
        <$> parseField "serverNames"
        <*> parseField "sslCertPath"
        <*> parseField "sslCertKeyPath"
        <*> parseField "letsencryptWwwDir"
        <*> parseField "port"