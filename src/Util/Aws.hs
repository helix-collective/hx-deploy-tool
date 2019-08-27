{-# LANGUAGE OverloadedStrings #-}
module Util.Aws(
  mkAwsEnv,
  mkAwsEnvFn,
  mkAwsEnv0,
  mkAwsEnvFn0,
  AwsEnv
  ) where

import qualified Log as L
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import Control.Concurrent.MVar(newEmptyMVar, tryReadMVar, putMVar)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.AWS(newEnv, envLogger, paginate, runAWST, configure, send, _ServiceError, serviceStatus, setEndpoint, Env, ServiceError, Credentials(..))
import System.Environment(lookupEnv)
import Types(IOR, REnv(..))
import Control.Monad.Reader(ask)
import Control.Lens(view, set, (&), (.~), (^?), (<&>))
import Network.URI(parseURI, URI(..), URIAuth(..))
import Network.AWS.S3(s3)

type AwsEnv = Env
type AwsEnvFn = IOR Env


-- Construct an AWS environment using the builtin
-- discovery mechanism for credentials. If the
-- S3_ENDPOINT environment variable is set as
--
--    http[s]://host:port
--
-- that will override the AWS default s3 endpoint.
mkAwsEnv0 :: IO AwsEnv
mkAwsEnv0 = do
  env <- newEnv Discover
  mS3Endpoint <- lookupEnv varName
  case mS3Endpoint of
    Nothing -> return env
    Just s3Endpoint -> case parseEndpoint s3Endpoint of
        Nothing -> error ("Unable to parse " <> varName)
        Just (isHttps,host,port) -> do
          let s3' = setEndpoint isHttps host port s3
          return env <&> configure s3'
  where
    varName = "S3_ENDPOINT"

    parseEndpoint :: String -> Maybe (Bool,BS.ByteString,Int)
    parseEndpoint uriStr =
     case parseURI uriStr of
       Just uri@URI{uriAuthority=Just auth} ->
         let isHttps = uriScheme uri == "https:"
             host = BS.pack (uriRegName auth)
             port = case uriPort auth of
               "" -> 80
               c:portStr -> read portStr
         in (Just (isHttps,host,port))
       Nothing -> Nothing

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

