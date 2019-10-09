{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as LT;
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import qualified Commands.LetsEncrypt as LE
import qualified Commands.ProxyMode as P
import qualified Commands as C
import qualified Util as U
import qualified Log as L

import ADL.Config(ToolConfig(..), LetsEncryptConfig(..), DeployMode(..))
import ADL.Core(adlFromByteString, AdlValue)
import Control.Exception(SomeException)
import Control.Monad.Catch(finally,catch)
import Control.Monad.Reader(runReaderT)
import Data.Monoid
import HelpText(helpText)
import Commands.ProxyMode.LocalState(nginxConfTemplate)
import System.Directory(doesFileExist)
import System.Environment(getArgs, lookupEnv, getExecutablePath)
import System.Exit(exitWith,ExitCode(..))
import System.FilePath(takeDirectory, takeExtension, (</>))
import System.Posix.Files(fileExist)
import Types(REnv(..),IOR, getToolConfig)
import Data.Version(showVersion)
import Paths_camus2(version)
import Util.Aws(mkAwsEnvFn0, AwsEnv)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["help"]                                    -> help
    ["--version"]                               -> putStrLn (showVersion version)
    ["list-releases"]                           -> runWithConfig       (C.listReleases)
    ["show-log"]                                -> runWithConfig       (C.showLog)
    ["show-default-nginx-config"]               -> C.showDefaultNginxConfig
    ["show-config-modes"]                       -> runWithConfig (C.listConfigsModes)
    ["show-config-modes", dynamicConfigName]    -> runWithConfig (C.showConfigModes (T.pack dynamicConfigName))

    ["fetch-context"]                           -> runWithConfigAndLog (U.fetchConfigContext Nothing)
    ["fetch-context","--retry"]                 -> runWithConfigAndLog (U.fetchConfigContext (Just 10))
    ["unpack", release, toDir]                  -> runWithConfigAndLog (U.unpackRelease id (T.pack release) toDir)
    ["expand-template", templatePath, destPath] -> runWithConfigAndLog (U.injectContext id templatePath destPath)
    ["aws-docker-login-cmd"]                    -> runWithConfigAndLog (C.awsDockerLoginCmd)
 
    ["status"]                                  -> runWithConfig       (P.showStatus False)
    ["status", "--show-slaves"]                 -> runWithConfig       (P.showStatus True)
    ["start", release]                          -> runWithConfigAndLog (C.createAndStart (T.pack release))
    ["stop", deploy]                            -> runWithConfigAndLog (C.stopDeploy (T.pack deploy))
    ["connect", endpoint, deploy]               -> runWithConfigAndLog (P.connect (T.pack endpoint) (T.pack deploy))
    ["disconnect", endpoint]                    -> runWithConfigAndLog (P.disconnect (T.pack endpoint))
    ["restart-frontend-proxy"]                  -> runWithConfigAndLog (P.restartProxy)
    ["generate-ssl-certificate"]                -> runWithConfigAndLog (P.generateSslCertificate)
    ["slave-flush"]                             -> runWithConfigAndLog (P.slaveFlush)
    ["slave-update"]                            -> runWithConfigAndLog (P.slaveUpdate Nothing)
    ["slave-update", "--repeat", ssecs]         -> do
      secs <- readCheck ssecs
      runWithConfigAndLog (P.slaveUpdate (Just secs))

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


readCheck :: (Read a) => String -> IO a
readCheck s = case reads s of
  [(a,"")] -> return a
  _ -> error ("unable to parse: " <> s)

-- | Load the config file and run the action
runWithConfig :: IOR () -> IO ()
runWithConfig ma = do
  tcfg <- loadToolConfig
  let logger = L.logger (L.logStdout L.Info)
  catch (runReaderT ma (REnv tcfg logger)) (ehandler logger)
  where
    ehandler logger e = L.error logger ("Exception: " <> LT.pack (show (e::SomeException)))

-- | Load the config file and run the action, writing log messages to the configured logfile
runWithConfigAndLog :: IOR () -> IO ()
runWithConfigAndLog ma = do
  tcfg <- loadToolConfig
  logToFile <-  L.logFile L.Info (T.unpack (tc_logFile tcfg))
  let logToStdout = L.logStdout L.Info
      logger = L.logger (L.combineLogFns logToFile logToStdout)
  finally (catch (runReaderT ma (REnv tcfg logger)) (ehandler logger)) (L.l_close (L.l_logfns logger))
  where
    ehandler logger e = L.error logger ("Exception: " <> LT.pack (show (e::SomeException)))

loadToolConfig :: IO ToolConfig
loadToolConfig = getConfig "CAMUS2_CONFIG" ["etc/camus2.json", "etc/camus2.yaml"]

getLetsEncryptConfig :: IO LetsEncryptConfig
getLetsEncryptConfig = getConfig "HX_LETSENCRYPT_CONFIG" ["etc/letsencrypt-aws.json"]

-- fetch an ADL config file, either from the path in the
-- given environment variable, or from a prefix relative
-- default path.
getConfig :: (AdlValue a) => String -> [FilePath] -> IO a
getConfig envVarName prefixPaths = do
  mEnvPath <- lookupEnv envVarName
  configPaths <- case mEnvPath of
   (Just configPath) -> return [configPath]
   Nothing -> do
     exePath <- getExecutablePath
     let prefix = takeDirectory (takeDirectory exePath)
     return [prefix </> path | path <- prefixPaths]
  getAwsEnv <- mkAwsEnvFn0
  mContent <- readFirst getAwsEnv configPaths
  case mContent of
    Nothing -> error ("Config file not found, tried: " <> show configPaths)
    (Just (configPath, lbs)) -> do
      let pr = parseContent configPath lbs
          from = "from " <> T.pack configPath
      case U.decodeAdlParseResult from pr of
        (Left err) -> error (T.unpack err)
        (Right a) -> return a
  where
    readFirst :: IO AwsEnv -> [FilePath] -> IO (Maybe (FilePath, LBS.ByteString))
    readFirst getAwsEnv [] = return Nothing
    readFirst getAwsEnv (path:paths) = do
      mlbs <- U.readFileOrS3 getAwsEnv path
      case mlbs of
        Nothing -> readFirst getAwsEnv paths
        Just lbs -> return (Just (path,lbs))

    parseContent configPath lbs = case takeExtension configPath of
      ".json" -> adlFromByteString lbs
      ".yaml" -> U.adlFromYamlByteString lbs
      _ -> error ("Unknown file type for config file: " <> configPath <> " (expected .json or .yaml)")

usageText :: T.Text
usageText = "\
  \General Usage:\n\
  \  c2 help\n\
  \  c2 list-releases\n\
  \  c2 show-log\n\
  \  c2 --version\n\
  \\n\
  \Deployment with a proxy:\n\
  \  c2 status [--show-slaves]\n\
  \  c2 start <release>\n\
  \  c2 stop <release>\n\
  \  c2 restart-frontend-proxy\n\
  \  c2 connect <endpoint> <release>\n\
  \  c2 disconnect <endpoint>\n\
  \\n\
  \Deployment without a proxy:\n\
  \  c2 select <release>\n\
  \\n\
  \Plumbing/Low Level Operations:\n\
  \  c2 fetch-context [--retry]\n\
  \  c2 unpack <release> <todir>\n\
  \  c2 expand-template <templatePath> <destPath>\n\
  \  c2 show-default-nginx-config\n\
  \  c2 aws-docker-login-cmd\n\
  \  c2 generate-ssl-certificate\n\
  \  c2 slave-flush\n\
  \  c2 slave-update [--repeat n]\n\
  \\n\
  \The config file is read from the file specified with CAMUS2_CONFIG.\n\
  \It defaults to ../etc/camus2.(json|yaml) relative to the executable.\n\
  \It is allowed to be an s3 path (ie s3://bucket/path).\n\
  \"
