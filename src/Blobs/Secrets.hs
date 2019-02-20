{-# LANGUAGE OverloadedStrings #-}
module Blobs.Secrets(
  secretExists,
  downloadSecretFrom
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.AWS.SecretsManager as SM
import qualified Network.AWS.SecretsManager.DescribeSecret as SM
import qualified Network.AWS.SecretsManager.GetSecretValue as SM

import Control.Exception.Lens
import Control.Lens
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Data.Conduit((.|), ($$+-), ConduitT, sealConduitT, runConduit)
import Network.HTTP.Types.Status(notFound404)

secretExists :: Env -> T.Text -> IO Bool
secretExists awsEnv secretArn = do
  handling _ServiceError onServiceError $ do
    runResourceT . runAWST awsEnv $ do
      send (SM.describeSecret secretArn)
      return True
  where
    onServiceError :: ServiceError -> IO Bool
    onServiceError se | view serviceStatus se == notFound404 = return False
                      | otherwise                            = throwing _ServiceError se

downloadSecretFrom :: Env -> T.Text -> FilePath -> IO ()
downloadSecretFrom awsEnv secretArn toFilePath = do
  runResourceT . runAWST awsEnv $ do
    resp <- send (SM.getSecretValue secretArn)
    case view SM.gsvrsSecretString resp of
      Nothing -> return ()
      (Just secret) -> liftIO $ T.writeFile toFilePath secret
