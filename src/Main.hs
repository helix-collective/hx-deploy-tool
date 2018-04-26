{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T

import ADL.Config(ToolConfig(..))
import ADL.Core(adlFromJsonFile')
import Commands(fetchContext, listReleases, select, unpack, awsDockerLoginCmd)
import System.Environment(getArgs, lookupEnv, getExecutablePath)
import System.Exit(exitWith,ExitCode(..))
import System.FilePath(takeDirectory, (</>))
import System.Posix.Files(fileExist)

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  hx-deploy-tool fetch-context [--retry]"
  putStrLn "  hx-deploy-tool list-releases"
  putStrLn "  hx-deploy-tool unpack <releaseid> <todir>"
  putStrLn "  hx-deploy-tool select <releaseid>"
  putStrLn "  hx-deploy-tool aws-docker-login-cmd"
  putStrLn ""
  putStrLn "The config file is read from the file specified with HX_DEPLOY_CONFIG."
  putStrLn "It defaults to ../etc/hx-deploy-tool.json (relative to the executable)."

getConfigPath :: IO FilePath
getConfigPath = do
  mConfigPath <- lookupEnv "HX_DEPLOY_CONFIG"
  case mConfigPath of
   (Just configPath) -> return configPath
   Nothing -> do
     exePath <- getExecutablePath
     return (takeDirectory (takeDirectory exePath) </> "etc/hx-deploy-tool.json")

main :: IO ()
main = do
  args <- getArgs
  configPath <- getConfigPath
  case args of
    ["fetch-context"] -> do
      config <- adlFromJsonFile' configPath
      fetchContext config Nothing
    ["fetch-context","--retry"] -> do
      config <- adlFromJsonFile' configPath
      fetchContext config (Just 10)
    ["list-releases"] -> do
      config <- adlFromJsonFile' configPath
      listReleases config
    ["unpack", release, toDir] -> do
      config <- adlFromJsonFile' configPath
      unpack config (T.pack release) toDir
    ["select", release] -> do
      config <- adlFromJsonFile' configPath
      select config (T.pack release)
    ["aws-docker-login-cmd"] -> do
      config <- adlFromJsonFile' configPath
      awsDockerLoginCmd config
    _ -> do
      usage
      exitWith (ExitFailure 1)
