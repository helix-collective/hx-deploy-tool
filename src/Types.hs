module Types where

import qualified Log as L
import qualified Data.Text as T;
import qualified Data.Text.Lazy as TL;

import ADL.Config(ToolConfig)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Reader(ReaderT, withReaderT, ask)

-- | The IOE type is a monad over IO, with associated
-- readable environment data
type IOR = ReaderT REnv IO

data REnv = REnv {
  re_toolConfig :: ToolConfig,
  re_logger :: L.Logger
}

-- | Extract the tool configuration from the environment
getToolConfig :: IOR ToolConfig
getToolConfig = fmap re_toolConfig ask

-- | Generate log messages
debug, info, lerror :: T.Text -> IOR ()
debug t = logm L.Debug (TL.fromStrict t)
info t = logm L.Info (TL.fromStrict t)
lerror t = logm L.Error (TL.fromStrict t)

-- | Flush the log - useful in a long running processs
flushlog :: IOR ()
flushlog = do
  logger <- fmap re_logger ask
  liftIO $ L.l_flush (L.l_logfns logger)

-- | Generate an info log message, indenting subsequently
-- log messages in child action.
scopeInfo :: T.Text -> IOR a -> IOR a
scopeInfo text ma = do
  info text
  withReaderT indentLogger ma
  where
    indentLogger re = re{re_logger=L.logIndent (re_logger re)}

-- | Generate a log message using the environment logger
logm :: L.LogLevel -> TL.Text -> IOR ()
logm level text = do
  logger <- fmap re_logger ask
  liftIO $ L.logm level logger text
