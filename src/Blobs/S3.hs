{-# LANGUAGE OverloadedStrings #-}
module Blobs.S3(
  splitPath,
  fileExists,
  downloadFileFrom,
  listFiles,
  extendObjectKey,
  mkAwsEnv,
  mkAwsEnvFn
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Network.AWS.S3 as S3
import qualified Data.Conduit.List as CL

import Data.Conduit((.|), ($$+-), ConduitT, sealConduitT, runConduit)
import Data.Conduit.Binary(sinkFile)
import Data.Monoid
import Data.Maybe(catMaybes)
import Data.List(sortOn)
import Data.Traversable(for)
import Control.Concurrent(threadDelay)
import Control.Concurrent.MVar
import Control.Exception.Lens
import Control.Lens
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(ask)
import Network.AWS.Data.Body(RsBody(..))
import Network.AWS.Data.Text(ToText(..))
import Network.HTTP.Types.Status(notFound404)

import qualified Log as L

import Types(IOR, REnv(..))

mkAwsEnv :: IOR Env
mkAwsEnv = do
  logger <- fmap re_logger ask
  liftIO $ do
    env0 <- newEnv Discover
    return (env0 & envLogger .~ (L.awsLogger logger))

type AwsEnvFn = IOR Env

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


splitPath :: T.Text -> (S3.BucketName,S3.ObjectKey)
splitPath s3Path = case T.stripPrefix "s3://" s3Path of
  Nothing -> error "s3Path must start with s3://"
  (Just s) ->
    case T.breakOn "/" s of
      (_,"") -> error "s3Path must include a key"
      (b,k) -> (S3.BucketName b,S3.ObjectKey (T.tail k))

fileExists :: Env -> S3.BucketName -> S3.ObjectKey -> IO Bool
fileExists env bucketName objectKey = do
  handling _ServiceError onServiceError $ do
    runResourceT . runAWST env $ do
      send (S3.headObject bucketName objectKey)
      return True
  where
    onServiceError :: ServiceError -> IO Bool
    onServiceError se | view serviceStatus se == notFound404 = return False
                      | otherwise                            = throwing _ServiceError se

downloadFileFrom :: Env -> S3.BucketName -> S3.ObjectKey -> FilePath -> Maybe Int -> IO ()
downloadFileFrom env bucketName  objectKey toFilePath retryAfter = do
  handling _ServiceError onServiceError $ do
    runResourceT . runAWST env $ do
      resp <- send (S3.getObject bucketName objectKey)
      let src = _streamBody (view S3.gorsBody resp) :: ConduitT () BS.ByteString (ResourceT IO) ()
          sink = sinkFile toFilePath :: ConduitT BS.ByteString o0 (ResourceT IO) ()
          stream = (sealConduitT src) $$+- sink
      liftResourceT stream
  where
   onServiceError :: ServiceError -> IO ()
   onServiceError se =
     case retryAfter of
        Nothing -> throwing _ServiceError se
        (Just delaySecs) -> do
          threadDelay (1000000 * delaySecs)
          downloadFileFrom env bucketName objectKey toFilePath retryAfter

listFiles :: Env -> S3.BucketName -> S3.ObjectKey -> IO [T.Text]
listFiles env bucketName (S3.ObjectKey prefix) = do
    let listObjectReq = set S3.loPrefix (Just prefix) (S3.listObjects bucketName)
    runResourceT . runAWST env $ do
      objects <- runConduit (paginate listObjectReq .| CL.foldMap (view S3.lorsContents))
      let sortedObjects = reverse (sortOn (view S3.oLastModified) objects)
      return (catMaybes (map (\o -> view S3.oKey o ^? S3.keyName '/') sortedObjects))

extendObjectKey :: S3.ObjectKey -> T.Text -> S3.ObjectKey
extendObjectKey (S3.ObjectKey prefix) extra = S3.ObjectKey (prefix <> extra)
