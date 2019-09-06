{-# LANGUAGE OverloadedStrings #-}
module ADL.Types(
    ConfigName,
    DeployLabel,
    EndPointLabel,
    FilePath,
    S3Path,
    StaticConfigName,
    StringKeyMap,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import Prelude( ($) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type ConfigName = T.Text

type DeployLabel = T.Text

type EndPointLabel = T.Text

type FilePath = T.Text

type S3Path = T.Text

type StaticConfigName = ConfigName

type StringKeyMap key value = StringMap (value)