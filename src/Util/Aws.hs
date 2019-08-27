{-# LANGUAGE OverloadedStrings #-}
module Util.Aws(
  mkAwsEnv,
  mkAwsEnvFn,
  mkAwsEnv0,
  mkAwsEnvFn0,
  AwsEnv
  ) where

import qualified Log as L

import Control.Concurrent.MVar(newEmptyMVar, tryReadMVar, putMVar)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.AWS(newEnv, envLogger, paginate, runAWST, send, _ServiceError,serviceStatus , Env, ServiceError, Credentials(..))
import Types(IOR, REnv(..))
import Control.Monad.Reader(ask)
import Control.Lens(view, set, (&), (.~), (^?))

type AwsEnv = Env
type AwsEnvFn = IOR Env


-- Construct an AWS environment using the builtin
-- discover mechanism for credentials
mkAwsEnv0 :: IO AwsEnv
mkAwsEnv0 = do
  env <- newEnv Discover
  return env

-- Construct an AWS environment, configuring the AWS
-- logging to use the application logger embedded in the IOR
-- monad
mkAwsEnv :: IOR AwsEnv
mkAwsEnv = do
  logger <- fmap re_logger ask
  liftIO $ do
    env0 <- mkAwsEnv0
    return (env0 & envLogger .~ (L.awsLogger logger))

-- Create an AWS environment when required, and reuse it subsequently.
mkAwsEnvFn :: IOR AwsEnvFn
mkAwsEnvFn = do
  mv <- liftIO $ newEmptyMVar
  return $ do
    menv <- liftIO $ tryReadMVar mv
    case menv of
      (Just awsEnv) -> return awsEnv
      Nothing -> do
        awsEnv <- mkAwsEnv
        liftIO $ putMVar mv awsEnv
        return awsEnv

-- Create an AWS environment when required, and reuse it subsequently.
-- (in the IO monad for bootstrap purposes)
mkAwsEnvFn0 :: IO (IO Env)
mkAwsEnvFn0 = do
  mv <- liftIO $ newEmptyMVar
  return $ do
    menv <- liftIO $ tryReadMVar mv
    case menv of
      (Just awsEnv) -> return awsEnv
      Nothing -> do
        awsEnv <- mkAwsEnv0
        liftIO $ putMVar mv awsEnv
        return awsEnv

