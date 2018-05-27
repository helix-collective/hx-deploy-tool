{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as CBS
import qualified Commands.LetsEncrypt as LE
import qualified Commands.ProxyMode as P
import qualified Commands as C

import ADL.Config(ToolConfig(..), LetsEncryptConfig(..))
import ADL.Core(adlFromJsonFile', AdlValue)
import Control.Monad.Reader(runReaderT)
import HelpText(helpText)
import System.Environment(getArgs, lookupEnv, getExecutablePath)
import System.Exit(exitWith,ExitCode(..))
import System.FilePath(takeDirectory, (</>))
import System.Posix.Files(fileExist)
import Types(REnv(..),IOR)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["help"]                            -> help
    ["fetch-context"]                   -> runWithConfig (C.fetchDeployContext Nothing)
    ["fetch-context","--retry"]         -> runWithConfig (C.fetchDeployContext (Just 10))
    ["list-releases"]                   -> runWithConfig (C.listReleases)
    ["unpack", release, toDir]          -> runWithConfig (C.unpackRelease' (T.pack release) toDir)
    ["select", release]                 -> runWithConfig (C.select (T.pack release))
    ["show-log"]                        -> runWithConfig (C.showLog)
    ["aws-docker-login-cmd"]            -> runWithConfig (C.awsDockerLoginCmd)
    ["proxy-status"]                    -> runWithConfig (P.showStatus)
    ["proxy-deploy", release]           -> runWithConfig (P.deploy (T.pack release))
    ["proxy-undeploy", deploy]          -> runWithConfig (P.undeploy (T.pack deploy))
    ["proxy-connect", endpoint, deploy] -> runWithConfig (P.connect (T.pack endpoint) (T.pack deploy))
    ["proxy-disconnect", endpoint]      -> runWithConfig (P.disconnect (T.pack endpoint))

    ["le-get-certs"] -> do
      config <- getLetsEncryptConfig
      LE.getCerts config
    ["le-auth-hook"] -> do
      config <- getLetsEncryptConfig
      LE.authHook config
    ["le-cleanup-hook"] -> do
      config <- getLetsEncryptConfig
      LE.cleanupHook config
    _ -> do
      usage
      exitWith (ExitFailure 10)


usage :: IO ()
usage = do
  T.putStrLn usageText

help :: IO ()
help = do
  CBS.putStrLn helpText

runWithConfig :: IOR a -> IO a
runWithConfig ma = do
  config <- getToolConfig
  runReaderT ma (REnv config)

getToolConfig :: IO ToolConfig
getToolConfig = getConfig "HX_DEPLOY_CONFIG" "etc/hx-deploy-tool.json"

getLetsEncryptConfig :: IO LetsEncryptConfig
getLetsEncryptConfig = getConfig "HX_LETSENCRYPT_CONFIG" "etc/letsencrypt-aws.json"

-- fetch an ADL config file, either from the path in the
-- given environment variable, or from a prefix relative
-- default path.
getConfig :: (AdlValue a) => String -> FilePath -> IO a
getConfig envVarName pathFromPrefix = do
  mEnvPath <- lookupEnv envVarName
  configPath <- case mEnvPath of
   (Just configPath) -> return configPath
   Nothing -> do
     exePath <- getExecutablePath
     return (takeDirectory (takeDirectory exePath) </> pathFromPrefix )
  adlFromJsonFile' configPath


usageText :: T.Text
usageText = "\
  \General Usage:\n\
  \  hx-deploy-tool help\n\
  \  hx-deploy-tool fetch-context [--retry]\n\
  \  hx-deploy-tool list-releases\n\
  \  hx-deploy-tool unpack <release> <todir>\n\
  \  hx-deploy-tool show-log\n\
  \  hx-deploy-tool aws-docker-login-cmd\n\
  \\n\
  \Deployment with a proxy:\n\
  \  hx-deploy-tool proxy-status\n\
  \  hx-deploy-tool proxy-deploy <release>\n\
  \  hx-deploy-tool proxy-undeploy <release>\n\
  \  hx-deploy-tool proxy-connect <endpoint> <release>\n\
  \  hx-deploy-tool proxy-disconnect <endpoint>\n\
  \\n\
  \Deployment without a proxy:\n\
  \  hx-deploy-tool select <release>\n\
  \\n\
  \The config file is read from the file specified with HX_DEPLOY_CONFIG.\n\
  \It defaults to ../etc/hx-deploy-tool.json (relative to the executable).\n\
  \"
