{-# LANGUAGE OverloadedStrings #-}
module ADL.Types(
    ConfigTopicName,
    DeployConfigTopicName,
    DeployLabel,
    EndPointLabel,
    FilePath,
    S3Path,
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

type DeployConfigTopicName = ConfigTopicName

type DeployLabel = T.Text

type EndPointLabel = T.Text

type FilePath = T.Text

type S3Path = T.Text

type StringKeyMap key value = StringMap (value)