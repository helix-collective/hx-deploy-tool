{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT;
import qualified Data.ByteString.Char8 as CBS
import qualified Commands.LetsEncrypt as LE
import qualified Commands.ProxyMode as P
import qualified Commands as C
import qualified Log as L

import ADL.Config(ToolConfig(..), LetsEncryptConfig(..))
import ADL.Core(adlFromJsonFile', AdlValue)
import Control.Exception(finally, catch, SomeException)
import Control.Monad.Reader(runReaderT)
import Data.Monoid
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
    ["list-releases"]                   -> runWithConfig       (C.listReleases)
    ["show-log"]                        -> runWithConfig       (C.showLog)

    ["fetch-context"]                   -> runWithConfigAndLog (C.fetchDeployContext Nothing)
    ["fetch-context","--retry"]         -> runWithConfigAndLog (C.fetchDeployContext (Just 10))
    ["unpack", release, toDir]          -> runWithConfigAndLog (C.unpackRelease' (T.pack release) toDir)
    ["aws-docker-login-cmd"]            -> runWithConfigAndLog (C.awsDockerLoginCmd)

    ["select", release]                 -> runWithConfigAndLog (C.select (T.pack release))

    ["proxy-status"]                    -> runWithConfig       (P.showStatus False)
    ["proxy-status", "--show-slaves"]   -> runWithConfig       (P.showStatus True)
    ["proxy-deploy", release]           -> runWithConfigAndLog (P.deploy (T.pack release))
    ["proxy-undeploy", deploy]          -> runWithConfigAndLog (P.undeploy (T.pack deploy))
    ["proxy-connect", endpoint, deploy] -> runWithConfigAndLog (P.connect (T.pack endpoint) (T.pack deploy))
    ["proxy-disconnect", endpoint]      -> runWithConfigAndLog (P.disconnect (T.pack endpoint))
    ["proxy-slave-update"]              -> runWithConfigAndLog (P.slaveUpdate)

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

-- | Load the config file and run the action
runWithConfig :: IOR () -> IO ()
runWithConfig ma = do
  tcfg <- getToolConfig
  let logger = L.logger (L.logStdout L.Info)
  catch (runReaderT ma (REnv tcfg logger)) (ehandler logger)
  where
    ehandler logger e = L.error logger ("Exception: " <> LT.pack (show (e::SomeException)))

-- | Load the config file and run the action, writing log messages to the configured logfile
runWithConfigAndLog :: IOR () -> IO ()
runWithConfigAndLog ma = do
  tcfg <- getToolConfig
  (logToFile,closeLogFile) <-  L.logFile L.Info (T.unpack (tc_logFile tcfg))
  let logToStdout = L.logStdout L.Info
      logger = L.logger (L.combineLogFns logToFile logToStdout)
  finally (catch (runReaderT ma (REnv tcfg logger)) (ehandler logger)) closeLogFile
  where
    ehandler logger e = L.error logger ("Exception: " <> LT.pack (show (e::SomeException)))

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
  \  hx-deploy-tool proxy-status [--show-slaves]\n\
  \  hx-deploy-tool proxy-deploy <release>\n\
  \  hx-deploy-tool proxy-undeploy <release>\n\
  \  hx-deploy-tool proxy-connect <endpoint> <release>\n\
  \  hx-deploy-tool proxy-disconnect <endpoint>\n\
  \  hx-deploy-tool proxy-slave-update\n\
  \\n\
  \Deployment without a proxy:\n\
  \  hx-deploy-tool select <release>\n\
  \\n\
  \The config file is read from the file specified with HX_DEPLOY_CONFIG.\n\
  \It defaults to ../etc/hx-deploy-tool.json (relative to the executable).\n\
  \"
