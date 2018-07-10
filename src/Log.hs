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
import System.IO(openFile, hClose, hFlush, stdout, IOMode(..))

data Logger  = Logger {
  l_prefix :: LT.Text,
  l_logfns :: LogFns
};

data LogFns = LogFns {
  l_log ::LogLevel -> LT.Text -> IO (),
  l_flush :: IO (),
  l_close :: IO ()
 }

data LogLevel = Debug | Info | Error
  deriving (Show,Eq,Ord);

logm :: LogLevel -> Logger -> LT.Text -> IO ()
logm level logger text = l_log (l_logfns logger) level (l_prefix logger <> text)

debug, info, error :: Logger -> LT.Text -> IO ()
debug = logm Debug
info = logm Info
error = logm Error

-- | Construct a logger
logger :: LogFns -> Logger
logger logfns = Logger "" logfns

-- | Create a logger with an additional indent marker
logIndent :: Logger -> Logger
logIndent (Logger prefix logf) = Logger ("." <> prefix) logf

-- | Create a log function writing to stdout
logStdout :: LogLevel -> LogFns
logStdout minLevel = LogFns log flush close
  where
    log level text | level >= minLevel = messageWithTimestamp text >>= LT.putStrLn
    log _ _ = return ()
    flush = hFlush stdout
    close = return ()

-- | Create a log function that appends to a file paired with an action
-- to close that file.
logFile :: LogLevel -> FilePath -> IO LogFns
logFile minLevel path = do
  h <- openFile path AppendMode
  return (LogFns (log h) (hFlush h) (hClose h))
  where
    log h level text | level >= minLevel = messageWithTimestamp text >>= LT.hPutStrLn h
    log _ _ _ = return ()

-- | Combine two log functions
combineLogFns :: LogFns -> LogFns -> LogFns
combineLogFns logfns1 logfns2 = LogFns log flush close
  where
    log level text = l_log logfns1 level text >> l_log logfns2 level text
    flush = l_flush logfns1 >> l_flush logfns2
    close = l_close logfns1 >> l_close logfns2

messageWithTimestamp :: LT.Text -> IO LT.Text
messageWithTimestamp text = do
  now <- getCurrentTime
  return (LT.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" now) <> " " <> text)

-- Returns an AWS logger that directs log messages to the specified Logger
awsLogger :: Logger -> AWS.Logger
awsLogger logger level builder = l_log (l_logfns logger) (toLevel level) (toLazyText builder)
  where
    toLevel :: AWS.LogLevel -> LogLevel
    toLevel AWS.Trace = Debug
    toLevel AWS.Debug = Debug
    toLevel AWS.Info = Info
    toLevel AWS.Error = Error

    toLazyText :: Builder -> LT.Text
    toLazyText = LT.decodeUtf8With LT.lenientDecode . toLazyByteString
