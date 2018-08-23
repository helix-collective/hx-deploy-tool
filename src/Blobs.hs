{-# LANGUAGE OverloadedStrings #-}
module Blobs where

import qualified Data.Text as T
import qualified Blobs.S3 as S3

import ADL.Config(ToolConfig(..), BlobStoreConfig(..))
import Data.Monoid
import Data.Traversable(for)
import Types(IOR, getToolConfig)
import System.Directory(listDirectory, doesFileExist, copyFile)
import System.FilePath((</>))

type BlobName = T.Text

data BlobStore = BlobStore {
  bs_label :: T.Text,
  bs_names :: IO [BlobName],
  bs_exists :: BlobName -> IO Bool,
  bs_fetchToFile :: BlobName -> FilePath -> IO ()
}

type S3Path = T.Text

releaseBlobStore :: IOR BlobStore
releaseBlobStore = do
  tcfg <- getToolConfig
  createBlobStore (tc_releases tcfg)

deployContextBlobStore :: IOR BlobStore
deployContextBlobStore = do
  tcfg <- getToolConfig
  createBlobStore (tc_deployContext tcfg)

createBlobStore :: BlobStoreConfig -> IOR BlobStore
createBlobStore (BlobStoreConfig_s3 s3Path) = awsBlobStore s3Path
createBlobStore (BlobStoreConfig_localdir dirpath)  = localBlobStore (T.unpack dirpath)

awsBlobStore :: S3Path -> IOR BlobStore
awsBlobStore s3Path = do
  env <- S3.mkAwsEnv
  return (BlobStore s3Path (bs_names env) (bs_exists env) (bs_fetchToFile env))
  where
    (bucketName,objectPrefix) = S3.splitPath s3Path
    objectKey blobname = S3.extendObjectKey objectPrefix ("/" <> blobname)

    bs_names env = do
      S3.listFiles env bucketName objectPrefix

    bs_exists env blobname = do
      S3.fileExists env bucketName (objectKey blobname)

    bs_fetchToFile env blobname filepath = do
      S3.downloadFileFrom env bucketName (objectKey blobname)  filepath Nothing


localBlobStore :: FilePath -> IOR BlobStore
localBlobStore path =
  return (BlobStore (T.pack path) bs_names bs_exists bs_fetchToFile)
  where
    bs_names = do
      names <- listDirectory path
      existing <- for names $ \name -> do
        e <- doesFileExist (path </> name)
        return (name,e)
      return [ T.pack name | (name,True) <- existing]

    bs_exists name = do
      doesFileExist (path </> T.unpack name)

    bs_fetchToFile name toFile = do
      copyFile (path </> T.unpack name) toFile
