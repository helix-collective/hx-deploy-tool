{-# LANGUAGE OverloadedStrings #-}
module Util.Aws.S3(
  adlValueFromS3,
  adlValueToS3,
  deleteObject,
  downloadFileFrom,
  extendObjectKey,
  listObjects,
  objectExists,
  readObject,
  splitPath,
  S3.BucketName,
  S3.ObjectKey,
  S3Metadata(..),
  S3Path,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Network.AWS.S3 as S3

import ADL.Core(adlFromByteString, adlToByteString, textFromParseContext, ParseResult(..), AdlValue)
import Control.Exception.Lens(handling, throwing)
import Control.Lens(view, set, (&), (.~), (^?), (<&>))
import Control.Monad.Trans.AWS(newEnv, envLogger, paginate, runAWST, send, _ServiceError,serviceStatus , Env, ServiceError, Credentials(..))
import Control.Monad.Trans.Resource(runResourceT, liftResourceT, ResourceT)
import Data.Conduit((.|), ($$+-), (=$), ConduitT, sealConduitT, runConduit)
import Data.Maybe(catMaybes)
import Data.List(sortOn)
import Data.Time(UTCTime)
import Network.AWS.Data.Body(RsBody(..),ToBody(..))
import Network.AWS.Data.Text(ToText(..))
import Network.HTTP.Types(notFound404)
import Util.Aws(AwsEnv)
import Data.Conduit.Combinators(sinkList)
import Data.Conduit.Binary(sinkFile)
import Control.Concurrent(threadDelay)

type S3Path = T.Text

data S3Metadata = S3Metadata {
  s3m_lastModified:: Maybe UTCTime
}
 
splitPath :: S3Path -> (S3.BucketName,S3.ObjectKey)
splitPath s3Path = case T.stripPrefix "s3://" s3Path of
  Nothing -> error "s3Path must start with s3://"
  (Just s) ->
    case T.breakOn "/" s of
      (_,"") -> error "s3Path must include a key"
      (b,k) -> (S3.BucketName b,S3.ObjectKey (T.tail k))

extendObjectKey :: S3.ObjectKey -> T.Text -> S3.ObjectKey
extendObjectKey (S3.ObjectKey prefix) extra = S3.ObjectKey (prefix <> extra)

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

deleteObject :: Env -> S3.BucketName -> S3.ObjectKey -> IO ()
deleteObject env bucketName objectKey = do
  handling _ServiceError onServiceError $ do
    runResourceT . runAWST env $ do
      send (S3.deleteObject bucketName objectKey)
      return ()
  where
    onServiceError :: ServiceError -> IO ()
    onServiceError se | view serviceStatus se == notFound404 = return ()
                      | otherwise                            = throwing _ServiceError se

listObjects :: Env -> S3.BucketName -> S3.ObjectKey -> IO [T.Text]
listObjects env bucketName (S3.ObjectKey prefix) = do
    let listObjectReq = set S3.loPrefix (Just prefix) (S3.listObjects bucketName)
    runResourceT . runAWST env $ do
      objects <- runConduit (paginate listObjectReq .| CL.foldMap (view S3.lorsContents))
      let sortedObjects = reverse (sortOn (view S3.oLastModified) objects)
      return (catMaybes (map (\o -> view S3.oKey o ^? S3.keyName '/') sortedObjects))

objectExists :: Env -> S3.BucketName -> S3.ObjectKey -> IO Bool
objectExists env bucketName objectKey = do
  handling _ServiceError onServiceError $ do
    runResourceT . runAWST env $ do
      send (S3.headObject bucketName objectKey)
      return True
  where
    onServiceError :: ServiceError -> IO Bool
    onServiceError se | view serviceStatus se == notFound404 = return False
                      | otherwise                            = throwing _ServiceError se

readObject :: Env -> S3.BucketName -> S3.ObjectKey -> IO LBS.ByteString
readObject env bucketName  objectKey = do
  runResourceT . runAWST env $ do
    resp <- send (S3.getObject bucketName objectKey)
    let src = _streamBody (view S3.gorsBody resp) :: ConduitT () BS.ByteString (ResourceT IO) ()
        stream = (sealConduitT src) $$+- sinkList
    LBS.fromChunks <$> liftResourceT stream

-- Write the specified ADL value to S3
adlValueToS3 :: (AdlValue a) => AwsEnv -> S3.BucketName -> S3.ObjectKey -> a -> IO ()
adlValueToS3 env bucketName objectKey a = do
  runResourceT . runAWST env $ do
    let bs = adlToByteString a
    send (S3.putObject bucketName objectKey (toBody bs))
    return ()

-- Read an ADL value from S3. Returns Nothing if there is
-- no value at the specified S3 key.
adlValueFromS3 :: (AdlValue a) => AwsEnv -> S3.BucketName -> S3.ObjectKey -> IO (Maybe (S3Metadata,ParseResult a))
adlValueFromS3 env bucketName objectKey = 
    handling _ServiceError onServiceError $ do
      runResourceT . runAWST env $ do
        resp <- send (S3.getObject bucketName objectKey)
        lbs <- fmap LBS.fromChunks $ liftResourceT (sealConduitT (_streamBody (view S3.gorsBody resp)) $$+- CL.consume)
        let mLastModified = view S3.gorsLastModified resp
            parseResult = adlFromByteString lbs
            metadata = S3Metadata mLastModified
        return (Just (metadata,parseResult))
  where
    onServiceError :: ServiceError -> IO (Maybe a)
    onServiceError se | view serviceStatus se == notFound404 = return Nothing
                      | otherwise                            = throwing _ServiceError se

