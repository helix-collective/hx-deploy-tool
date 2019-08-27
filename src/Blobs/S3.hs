{-# LANGUAGE OverloadedStrings #-}
module Blobs.S3(
  splitPath,
  fileExists,
  readFile,
  downloadFileFrom,
  listFiles,
  extendObjectKey,
  deleteFile
  ) where

import Prelude hiding (readFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Network.AWS.S3 as S3
import qualified Data.Conduit.List as CL
import qualified Log as L

import Data.Conduit((.|), ($$+-), ConduitT, sealConduitT, runConduit)
import Data.Conduit.Combinators(sinkList)
import Data.Conduit.Binary(sinkFile)
import Data.Maybe(catMaybes)
import Data.List(sortOn)
import Data.Traversable(for)
import Control.Concurrent(threadDelay)
import Control.Exception.Lens(handling, throwing)
import Control.Lens(view, set, (&), (.~), (^?))
import Control.Monad.Trans.AWS(newEnv, envLogger, paginate, runAWST, send, _ServiceError,serviceStatus , Env, ServiceError, Credentials(..))
import Control.Monad.Trans.Resource(runResourceT, liftResourceT, ResourceT)
import Network.AWS.Data.Body(RsBody(..))
import Network.AWS.Data.Text(ToText(..))
import Network.HTTP.Types.Status(notFound404)
import Types(IOR, REnv(..))

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

readFile :: Env -> S3.BucketName -> S3.ObjectKey -> IO LBS.ByteString
readFile env bucketName  objectKey = do
  runResourceT . runAWST env $ do
    resp <- send (S3.getObject bucketName objectKey)
    let src = _streamBody (view S3.gorsBody resp) :: ConduitT () BS.ByteString (ResourceT IO) ()
        stream = (sealConduitT src) $$+- sinkList
    LBS.fromChunks <$> liftResourceT stream

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

deleteFile :: Env -> S3.BucketName -> S3.ObjectKey -> IO ()
deleteFile env bucketName objectKey = do
  handling _ServiceError onServiceError $ do
    runResourceT . runAWST env $ do
      send (S3.deleteObject bucketName objectKey)
      return ()
  where
    onServiceError :: ServiceError -> IO ()
    onServiceError se | view serviceStatus se == notFound404 = return ()
                      | otherwise                            = throwing _ServiceError se

listFiles :: Env -> S3.BucketName -> S3.ObjectKey -> IO [T.Text]
listFiles env bucketName (S3.ObjectKey prefix) = do
    let listObjectReq = set S3.loPrefix (Just prefix) (S3.listObjects bucketName)
    runResourceT . runAWST env $ do
      objects <- runConduit (paginate listObjectReq .| CL.foldMap (view S3.lorsContents))
      let sortedObjects = reverse (sortOn (view S3.oLastModified) objects)
      return (catMaybes (map (\o -> view S3.oKey o ^? S3.keyName '/') sortedObjects))

extendObjectKey :: S3.ObjectKey -> T.Text -> S3.ObjectKey
extendObjectKey (S3.ObjectKey prefix) extra = S3.ObjectKey (prefix <> extra)
