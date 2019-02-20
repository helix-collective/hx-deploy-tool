{-# LANGUAGE OverloadedStrings #-}
module Commands.ProxyMode.RemoteState(
  remoteState,
  writeSlaveState,
  masterS3Path,
  slaveS3Path,
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
import Data.Traversable(for)
import Data.Maybe(catMaybes)
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
  stateFromS3 env (masterS3Path remoteStateS3)

getSlaves :: S3Path -> IOR [(T.Text, State)]
getSlaves remoteStateS3 = do
  env <- S3.mkAwsEnv
  labels <- getSlaveLabels env
  for labels $ \label -> do
    state <- stateFromS3 env (slaveS3Path remoteStateS3 label)
    return (label,state)
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

updateState :: S3Path -> (State -> State) -> IOR ()
updateState remoteStateS3 modf = do
  let s3Path = masterS3Path remoteStateS3
  info ("Updating remote state at " <> s3Path)
  env <- S3.mkAwsEnv
  state <- stateFromS3 env s3Path
  let state' = modf state
  stateToS3 env s3Path state'

writeSlaveState :: S3Path -> T.Text -> State -> IOR ()
writeSlaveState remoteStateS3 label state = do
  env <- S3.mkAwsEnv
  stateToS3 env (slaveS3Path remoteStateS3 label) state

stateFromS3 :: Env -> S3Path -> IOR State
stateFromS3 env s3Path = do
  let (bucketName,objectKey) = S3.splitPath s3Path
  liftIO $ do
    handling _ServiceError onServiceError $ do
      runResourceT . runAWST env $ do
        resp <- send (S3.getObject bucketName objectKey)
        lbs <- fmap LBS.fromChunks $ liftResourceT (sealConduitT (_streamBody (view S3.gorsBody resp)) $$+- CL.consume)
        case adlFromByteString lbs of
          (ParseFailure e ctx)
            -> error (T.unpack ("Failed to parse state adl at " <> s3Path <> ": " <> e <> " at " <> textFromParseContext ctx))
          (ParseSuccess state)
            -> return state
  where
    onServiceError :: ServiceError -> IO State
    onServiceError se | view serviceStatus se == notFound404 = return emptyState
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
