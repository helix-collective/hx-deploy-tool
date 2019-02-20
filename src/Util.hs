{-# LANGUAGE OverloadedStrings #-}
module Util where

import qualified ADL.Core.StringMap as SM
import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy as LT
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Log as L
import qualified Text.Mustache as TM
import qualified Text.Mustache.Types as TM

import ADL.Config(ToolConfig(..), DeployContextFile(..), DeployMode(..), ProxyModeConfig(..))
import ADL.Release(ReleaseConfig(..))
import ADL.Core(adlFromJsonFile')
import Blobs(releaseBlobStore, deployContextBlobStore, BlobStore(..), BlobName)
import Codec.Archive.Zip(withArchive, unpackInto)
import Control.Concurrent(threadDelay)
import Control.Monad(when)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.List(sortOn)
import Data.Maybe(fromMaybe)
import Data.Monoid
import Data.Foldable(for_)
import Data.Time.Clock.POSIX(getCurrentTime)
import Data.Traversable(for)
import System.Directory(createDirectoryIfMissing,doesFileExist,doesDirectoryExist,withCurrentDirectory)
import System.FilePath(takeBaseName, takeDirectory, dropExtension, (</>))
import System.Posix.Files(createSymbolicLink, removeLink)
import System.IO(stdout, withFile, hIsEOF, IOMode(..))
import Path(Path,Abs,Dir,File,parseAbsDir,parseAbsFile)
import Types(IOR, REnv(..), getToolConfig, scopeInfo, info)

--- Download the infrastructure context files from the blobstore
fetchDeployContext :: Maybe Int -> IOR ()
fetchDeployContext retryAfter = do
  scopeInfo "Fetching deploy context from store" $ do
    bs <- deployContextBlobStore
    tcfg <- getToolConfig
    do
      let cacheDir = T.unpack (tc_contextCache tcfg)
      liftIO $ createDirectoryIfMissing True cacheDir
      for_ (tc_deployContextFiles tcfg) $ \cf -> do
        let cacheFilePath = cacheDir </> T.unpack (dcf_name cf)
        case retryAfter of
          Nothing -> return ()
          Just delay -> await bs (dcf_sourceName cf) delay
        info ("Downloading " <> dcf_sourceName cf <> " from " <> bs_label bs <> " to " <> T.pack cacheFilePath)
        liftIO $ bs_fetchToFile bs (dcf_sourceName cf) cacheFilePath
 where
   await:: BlobStore -> BlobName -> Int -> IOR ()
   await bs name delaySecs = do
     exists <- liftIO $ bs_exists bs name
     case exists of
       True -> return ()
       False -> do
         info ("Waiting for "<> name <> " in " <> bs_label bs)
         liftIO $ threadDelay (1000000 * delaySecs)
         await bs name delaySecs

-- unpack a release into the specified directory, and expand any templates
--
-- `modifyContextFn` can be used to modify the context before it is used to
-- expand templates
unpackRelease :: (JS.Value -> JS.Value) -> T.Text -> FilePath -> IOR ()
unpackRelease modifyContextFn release toDir = do
  scopeInfo ("Unpacking release " <> " to " <> T.pack toDir) $ do
    tcfg <- getToolConfig
    bs <- releaseBlobStore
    liftIO $ do
      let releaseFilePath = toDir </> T.unpack release
      createDirectoryIfMissing True toDir

      -- download the release zip file
      bs_fetchToFile bs release releaseFilePath

      -- expand the zip file
      withArchive releaseFilePath (unpackInto toDir)

      -- load the release metadata
      rcfg <- adlFromJsonFile' (toDir </> "release.json")

      -- load and merge the infrastructure context
      ctx <- fmap modifyContextFn (loadMergedContext tcfg)

      -- interpolate the context into each templated file
      for_ (rc_templates rcfg) $ \templatePath -> do
        expandTemplateFile ctx (toDir </> T.unpack templatePath)

checkReleaseExists :: T.Text -> IOR ()
checkReleaseExists release = do
  bs <- releaseBlobStore
  exists <- liftIO $ bs_exists bs release
  when (not exists) $ do
    error ("Release " <> T.unpack release <> " does not exist in S3")
  return ()

loadMergedContext :: ToolConfig -> IO JS.Value
loadMergedContext tcfg = do
  let cacheDir = T.unpack (tc_contextCache tcfg)
  values <- for (tc_deployContextFiles tcfg) $ \cf -> do
    let cacheFilePath = cacheDir </> T.unpack (dcf_name cf)
    lbs <- LBS.readFile cacheFilePath
    case JS.eitherDecode' lbs of
     (Left e) -> error ("Unable to parse json from " <> cacheFilePath)
     (Right jv) -> return (takeBaseName cacheFilePath, jv)
  return (JS.Object (HM.fromList [(T.pack file,jv) | (file,jv) <- values]))

expandTemplateFile :: JS.Value -> FilePath -> IO ()
expandTemplateFile ctx templatePath = do
  etemplate <- TM.automaticCompile [takeDirectory templatePath] templatePath
  case etemplate of
   Left err -> error (show err)
   Right template -> do
     let text = TM.substitute template ctx
         outfile = dropExtension templatePath
     T.writeFile outfile text

toDirPath :: FilePath -> Path Abs Dir
toDirPath path = case parseAbsDir path of
  Just p -> p
  Nothing -> error "Unable to parse directory"

toFilePath :: FilePath -> Path Abs File
toFilePath path = case parseAbsFile path of
  Just p -> p
  Nothing -> error "Unable to parse directory"
