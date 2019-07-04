{-# LANGUAGE OverloadedStrings #-}
module Commands.ProxyMode.RemoteState(
  remoteState,
  writeSlaveState,
  masterS3Path,
  slaveS3Path,
  flushSlaveStates,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as JS
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Conduit.List as CL
import qualified Network.AWS.S3 as S3
import qualified Blobs.S3 as S3

import Control.Exception.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS
import Control.Monad.Trans.Resource
import Control.Lens
import Data.Conduit((.|), ($$+-), (=$), sealConduitT, runConduit)
import Data.Time(UTCTime)
import Data.Traversable(for)
import Data.Maybe(catMaybes)
import Data.Foldable(for_)
import Control.Monad(when)
import Data.Monoid
import Network.AWS.Data.Body(RsBody(..))
import Network.AWS.Types(serviceStatus)
import Network.HTTP.Types(notFound404)

import ADL.Core(adlFromByteString, adlToByteString, textFromParseContext, ParseResult(..))
import ADL.State(State(..), Deploy(..))
import Commands.ProxyMode.Types
import Types(IOR, REnv(..), getToolConfig, scopeInfo, info)

type S3Path = T.Text

remoteState :: S3Path -> StateAccess
remoteState  remoteStateS3 = StateAccess {
  sa_get=getState remoteStateS3,
  sa_getSlaves=getSlaves remoteStateS3 ,
  sa_update=updateState remoteStateS3
}

getState :: S3Path -> IOR State
getState remoteStateS3 = do
  env <- S3.mkAwsEnv
  s3s_state <$> stateFromS3 env (masterS3Path remoteStateS3)

getSlaves :: S3Path -> IOR [(T.Text, SlaveState)]
getSlaves remoteStateS3 = do
  env <- S3.mkAwsEnv
  labels <- getSlaveLabels env
  for labels $ \label -> do
    state <- stateFromS3 env (slaveS3Path remoteStateS3 label)
    return (label,mkSlaveState state)
  where
    getSlaveLabels :: Env -> IOR [T.Text]
    getSlaveLabels env = do
      let (bucketName,S3.ObjectKey bucketPath) = S3.splitPath (remoteStateS3 <> "/slaves")
          listObjectReq = set S3.loPrefix (Just bucketPath) (S3.listObjects bucketName)
      objects <- runResourceT . runAWST env $ do
        runConduit (paginate listObjectReq .| CL.foldMap (view S3.lorsContents))
      let keys = catMaybes [view S3.oKey obj ^? S3._ObjectKey | obj <- objects]
      let labels = catMaybes (map parseSlaveLabel keys)
      return labels

    -- extract label from ".../label/state.json"
    parseSlaveLabel :: T.Text -> Maybe T.Text
    parseSlaveLabel key = case T.stripSuffix "/state.json" key of
      Nothing -> Nothing
      (Just s) -> Just (T.takeWhileEnd (/='/') s)

    mkSlaveState (S3State state mLastModified) = SlaveState state mLastModified

updateState :: S3Path -> (State -> State) -> IOR ()
updateState remoteStateS3 modf = do
  let s3Path = masterS3Path remoteStateS3
  info ("Updating remote state at " <> s3Path)
  env <- S3.mkAwsEnv
  state <- s3s_state <$> stateFromS3 env s3Path
  let state' = modf state
  stateToS3 env s3Path state'

writeSlaveState :: S3Path -> T.Text -> State -> IOR ()
writeSlaveState remoteStateS3 label state = do
  env <- S3.mkAwsEnv
  stateToS3 env (slaveS3Path remoteStateS3 label) state

-- Remove slave states that haven't been updated since
-- the specified time
flushSlaveStates:: UTCTime -> S3Path -> IOR ()
flushSlaveStates notUpdatedSince remoteStateS3 = do
  info "flushing old slave states from s3"
  ls <- getSlaves remoteStateS3
  env <- S3.mkAwsEnv
  for_ ls $ \(label, sstate) -> do
    case ss_lastModified sstate of
      (Just lastModified) | lastModified < notUpdatedSince -> do
        let s3path = slaveS3Path remoteStateS3 label
        info ("removing old slave state at " <> s3path)
        let (bucketName,objectKey) = S3.splitPath s3path
        liftIO $ S3.deleteFile env bucketName objectKey
      _ -> return ()

data S3State = S3State {
  s3s_state :: State,
  s3s_lastModified :: Maybe UTCTime
}

stateFromS3 :: Env -> S3Path -> IOR S3State
stateFromS3 env s3Path = do
  let (bucketName,objectKey) = S3.splitPath s3Path
  liftIO $ do
    handling _ServiceError onServiceError $ do
      runResourceT . runAWST env $ do
        resp <- send (S3.getObject bucketName objectKey)
        lbs <- fmap LBS.fromChunks $ liftResourceT (sealConduitT (_streamBody (view S3.gorsBody resp)) $$+- CL.consume)
        let mLastModified = view S3.gorsLastModified resp
        case adlFromByteString lbs of
          (ParseFailure e ctx)
            -> error (T.unpack ("Failed to parse state adl at " <> s3Path <> ": " <> e <> " at " <> textFromParseContext ctx))
          (ParseSuccess state)
            -> return (S3State state mLastModified)
  where
    onServiceError :: ServiceError -> IO S3State
    onServiceError se | view serviceStatus se == notFound404 = return (S3State emptyState Nothing)
                      | otherwise                            = throwing _ServiceError se

stateToS3 :: Env -> S3Path -> State -> IOR ()
stateToS3 env s3Path state = do
  let (bucketName,objectKey) = S3.splitPath s3Path
  liftIO $ do
    runResourceT . runAWST env $ do
      let bs = adlToByteString state
      send (S3.putObject bucketName objectKey (toBody bs))
      return ()

masterS3Path :: S3Path -> S3Path
masterS3Path base = base <> "/master/state.json"

slaveS3Path :: S3Path -> T.Text -> S3Path
slaveS3Path base label = base <> "/slaves/" <> label <> "/state.json"
