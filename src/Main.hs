{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import ADL.Config(ToolConfig)
import ADL.Core(adlFromJsonFile')
import System.Environment(getArgs, getEnv, getExecutablePath)
import System.Exit(exitWith,ExitCode(..))
import System.FilePath(takeDirectory, (</>))

fetchContext :: ToolConfig -> IO ()
fetchContext tfg = undefined

select :: ToolConfig -> T.Text -> IO ()
select = undefined

unpack :: ToolConfig -> T.Text -> IO ()
unpack = undefined

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  hx-deploy-tool fetch-context"
  putStrLn "  hx-deploy-tool unpack <releaseid>"
  putStrLn "  hx-deploy-tool select <releaseid>"

getConfigPath :: IO FilePath
getConfigPath = do
  exePath <- getExecutablePath
  return (takeDirectory (takeDirectory exePath) </> "etc/hx-deploy-tool.json")

main :: IO ()
main = do
  args <- getArgs
  configPath <- getConfigPath
  case args of
    ["fetch-context"] -> do
      config <- adlFromJsonFile' configPath
      fetchContext config
    ["unpack", release] -> do
      config <- adlFromJsonFile' configPath
      unpack config (T.pack release)
    ["select", release] -> do
      config <- adlFromJsonFile' configPath
      select config (T.pack release)
    _ -> do
      usage
      exitWith (ExitFailure 1)
