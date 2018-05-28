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
debug, info, error :: TL.Text -> IOR ()
debug = logm L.Debug
info = logm L.Info
error = logm L.Error

-- | Generate an info log message, indenting subsequently
-- log messages in child action.
scopeInfo :: T.Text -> IOR a -> IOR a
scopeInfo text ma = do
  info (TL.fromStrict text)
  withReaderT indentLogger ma
  where
    indentLogger re = re{re_logger=L.logIndent (re_logger re)}

-- | Generate a log message using the environment logger
logm :: L.LogLevel -> TL.Text -> IOR ()
logm level text = do
  logger <- fmap re_logger ask
  liftIO $ L.logm level logger text
