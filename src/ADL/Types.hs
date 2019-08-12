{-# LANGUAGE OverloadedStrings #-}
module ADL.Types(
    ConfigTopicName,
    DeployLabel,
    EndPointLabel,
    FilePath,
    S3Path,
    StaticConfigTopicName,
    StringKeyMap,
) where

import ADL.Core
import Control.Applicative( (<$>), (<*>), (<|>) )
import qualified Data.Aeson as JS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Proxy
import qualified Data.Text as T
import qualified Prelude

type ConfigTopicName = T.Text

type DeployLabel = T.Text

type EndPointLabel = T.Text

type FilePath = T.Text

type S3Path = T.Text

type StaticConfigTopicName = ConfigTopicName

type StringKeyMap key value = StringMap (value)