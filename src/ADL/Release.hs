{-# LANGUAGE OverloadedStrings #-}
module ADL.Release(
    ReleaseConfig(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified ADL.Types
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

data ReleaseConfig = ReleaseConfig
    { rc_templates :: [ADL.Types.FilePath]
    , rc_prestartCommand :: T.Text
    , rc_startCommand :: T.Text
    , rc_stopCommand :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkReleaseConfig :: [ADL.Types.FilePath] -> T.Text -> T.Text -> T.Text -> ReleaseConfig
mkReleaseConfig templates prestartCommand startCommand stopCommand = ReleaseConfig templates prestartCommand startCommand stopCommand

instance AdlValue ReleaseConfig where
    atype _ = "release.ReleaseConfig"
    
    jsonGen = genObject
        [ genField "templates" rc_templates
        , genField "prestartCommand" rc_prestartCommand
        , genField "startCommand" rc_startCommand
        , genField "stopCommand" rc_stopCommand
        ]
    
    jsonParser = ReleaseConfig
        <$> parseField "templates"
        <*> parseField "prestartCommand"
        <*> parseField "startCommand"
        <*> parseField "stopCommand"