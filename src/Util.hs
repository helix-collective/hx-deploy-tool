{-# LANGUAGE OverloadedStrings #-}
module Util where

import qualified Data.Text as T
import qualified Log as L
import qualified Network.AWS.S3 as S3

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Lens
import Types(IOR, REnv(..), getToolConfig, scopeInfo)

mkAwsEnv :: IOR Env
mkAwsEnv = do
  logger <- fmap re_logger ask
  liftIO $ do
    env0 <- newEnv Discover
    return (env0 & envLogger .~ (L.awsLogger logger))

splitS3Path :: T.Text -> (S3.BucketName,S3.ObjectKey)
splitS3Path s3Path = case T.stripPrefix "s3://" s3Path of
  Nothing -> error "s3Path must start with s3://"
  (Just s) ->
    case T.breakOn "/" s of
      (_,"") -> error "s3Path must include a key"
      (b,k) -> (S3.BucketName b,S3.ObjectKey (T.tail k))
