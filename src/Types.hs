module Types where

import ADL.Config(ToolConfig)
import Control.Monad.Reader

-- The IOE type is a monad over IO, with associated
-- readable environment data
type IOR = ReaderT REnv IO

data REnv = REnv {
  re_toolConfig :: ToolConfig
}


getToolConfig :: IOR ToolConfig
getToolConfig = fmap re_toolConfig ask
