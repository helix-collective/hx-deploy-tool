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
import qualified Util.Aws as AWS
import qualified Util.Aws.S3 as S3

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
import Util(decodeAdlParseResult)

type S3Path = T.Text

remoteState :: S3Path -> StateAccess
remoteState  remoteStateS3 = StateAccess {
  sa_get=getState remoteStateS3,
  sa_getSlaves=getSlaves remoteStateS3 ,
  sa_update=updateState remoteStateS3
}

getState :: S3Path -> IOR State
getState remoteStateS3 = do
  env <- AWS.mkAwsEnv
  s3s_state <$> stateFromS3 env (masterS3Path remoteStateS3)

getSlaves :: S3Path -> IOR [(T.Text, SlaveState)]
getSlaves remoteStateS3 = do
  env <- AWS.mkAwsEnv
  labels <- getSlaveLabels env
  for labels $ \label -> do
    state <- stateFromS3 env (slaveS3Path remoteStateS3 label)
    return (label,mkSlaveState state)
  where
    getSlaveLabels :: Env -> IOR [T.Text]
    getSlaveLabels env = do
      let (bucketName,objectKey) = S3.splitPath (remoteStateS3 <> "/slaves")
      keys <- liftIO $ S3.listObjects env bucketName objectKey
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
  env <- AWS.mkAwsEnv
  state <- s3s_state <$> stateFromS3 env s3Path
  let state' = modf state
  stateToS3 env s3Path state'

writeSlaveState :: S3Path -> T.Text -> State -> IOR ()
writeSlaveState remoteStateS3 label state = do
  env <- AWS.mkAwsEnv
  stateToS3 env (slaveS3Path remoteStateS3 label) state

-- Remove slave states that haven't been updated since
-- the specified time
flushSlaveStates:: UTCTime -> S3Path -> IOR ()
flushSlaveStates notUpdatedSince remoteStateS3 = do
  info "flushing old slave states from s3"
  ls <- getSlaves remoteStateS3
  env <- AWS.mkAwsEnv
  for_ ls $ \(label, sstate) -> do
    case ss_lastModified sstate of
      (Just lastModified) | lastModified < notUpdatedSince -> do
        let s3path = slaveS3Path remoteStateS3 label
        info ("removing old slave state at " <> s3path)
        let (bucketName,objectKey) = S3.splitPath s3path
        liftIO $ S3.deleteObject env bucketName objectKey
      _ -> return ()

data S3State = S3State {
  s3s_state :: State,
  s3s_lastModified :: Maybe UTCTime
}

stateFromS3 :: Env -> S3Path -> IOR S3State
stateFromS3 env s3Path = do
  let (bucketName,objectKey) = S3.splitPath s3Path
  liftIO $ do
    mpr <- S3.adlValueFromS3 env bucketName objectKey
    case mpr of
      Nothing -> return (S3State emptyState Nothing)
      Just (s3m,pr) -> case decodeAdlParseResult s3Path pr of
        Left emsg -> error (T.unpack emsg)
        Right state -> return (S3State state (S3.s3m_lastModified s3m))

stateToS3 :: Env -> S3Path -> State -> IOR ()
stateToS3 env s3Path state = do
  let (bucketName,objectKey) = S3.splitPath s3Path
  liftIO $ S3.adlValueToS3 env bucketName objectKey state

masterS3Path :: S3Path -> S3Path
masterS3Path base = base <> "/master/state.json"

slaveS3Path :: S3Path -> T.Text -> S3Path
slaveS3Path base label = base <> "/slaves/" <> label <> "/state.json"
