{-# LANGUAGE OverloadedStrings #-}
module ADL.Release(
    FilePath,
    ReleaseConfig(..),
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type FilePath = T.Text

data ReleaseConfig = ReleaseConfig
    { releaseConfig_templates :: [FilePath]
    , releaseConfig_startScript :: T.Text
    , releaseConfig_stopScript :: T.Text
    }
    deriving (Prelude.Eq,Prelude.Ord,Prelude.Show)

mkReleaseConfig :: [FilePath] -> T.Text -> T.Text -> ReleaseConfig
mkReleaseConfig templates startScript stopScript = ReleaseConfig templates startScript stopScript

instance AdlValue ReleaseConfig where
    atype _ = "release.ReleaseConfig"
    
    jsonGen = genObject
        [ genField "templates" releaseConfig_templates
        , genField "startScript" releaseConfig_startScript
        , genField "stopScript" releaseConfig_stopScript
        ]
    
    jsonParser = ReleaseConfig
        <$> parseField "templates"
        <*> parseField "startScript"
        <*> parseField "stopScript"