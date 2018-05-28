{-# LANGUAGE OverloadedStrings #-}

module Log where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Encoding.Error as LT
import qualified Control.Monad.Trans.AWS as AWS
import qualified Data.ByteString.Lazy as LBS

import Data.Monoid
import Data.Binary.Builder(Builder,toLazyByteString)
import Data.Time.Clock(UTCTime, getCurrentTime)
import Data.Time.Format(formatTime, defaultTimeLocale)
import System.IO(openFile, hClose, IOMode(..))

data Logger  = Logger {
  l_prefix :: LT.Text,
  l_log :: LogFn
};

type LogFn = LogLevel -> LT.Text -> IO ()

data LogLevel = Debug | Info | Error
  deriving (Show,Eq,Ord);

logm :: LogLevel -> Logger -> LT.Text -> IO ()
logm level logger text = l_log logger level (l_prefix logger <> text)

debug, info, error :: Logger -> LT.Text -> IO ()
debug = logm Debug
info = logm Info
error = logm Error

-- | Construct a logger
logger :: LogFn -> Logger
logger logfn = Logger "" logfn

-- | Create a logger with an additional indent marker
logIndent :: Logger -> Logger
logIndent (Logger prefix logf) = Logger ("." <> prefix) logf

-- | Create a log function writing to stdout
logStdout :: LogLevel -> LogFn
logStdout minLevel = logfn
  where
    logfn level text | level >= minLevel = messageWithTimestamp text >>= LT.putStrLn
    logfn _ _ = return ()

-- | Create a log function that appends to a file paired with an action
-- to close that file.
logFile :: LogLevel -> FilePath -> IO (LogFn, IO ())
logFile minLevel path = do
  h <- openFile path AppendMode
  return (logfn h, hClose h)
  where
    logfn h level text | level >= minLevel = messageWithTimestamp text >>= LT.hPutStrLn h
    logfn _ _ _ = return ()

-- | Combine two log functions
combineLogFns :: LogFn -> LogFn -> LogFn
combineLogFns logfn1 logfn2 level text = logfn1 level text >> logfn2 level text

messageWithTimestamp :: LT.Text -> IO LT.Text
messageWithTimestamp text = do
  now <- getCurrentTime
  return (LT.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now) <> " " <> text)

-- Returns an AWS logger that directs log messages to the specified Logger
awsLogger :: Logger -> AWS.Logger
awsLogger logger level builder = l_log logger (toLevel level) (toLazyText builder)
  where
    toLevel :: AWS.LogLevel -> LogLevel
    toLevel AWS.Trace = Debug
    toLevel AWS.Debug = Debug
    toLevel AWS.Info = Info
    toLevel AWS.Error = Error

    toLazyText :: Builder -> LT.Text
    toLazyText = LT.decodeUtf8With LT.lenientDecode . toLazyByteString
