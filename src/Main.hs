{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Commands.LetsEncrypt as LE
import qualified Commands.ProxyMode as P
import qualified Commands as C

import ADL.Config(ToolConfig(..), LetsEncryptConfig(..))
import ADL.Core(adlFromJsonFile', AdlValue)
import Control.Monad.Reader(runReaderT)
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
  T.putStrLn helpText

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
  \Deployment Usage:\n\
  \  hx-deploy-tool help\n\
  \  hx-deploy-tool fetch-context [--retry]\n\
  \  hx-deploy-tool list-releases\n\
  \  hx-deploy-tool unpack <release> <todir>\n\
  \  hx-deploy-tool select <release>\n\
  \  hx-deploy-tool show-log\n\
  \  hx-deploy-tool aws-docker-login-cmd\n\
  \\n\
  \The config file is read from the file specified with HX_DEPLOY_CONFIG.\n\
  \It defaults to ../etc/hx-deploy-tool.json (relative to the executable).\n\
  \\n\
  \Cert Generation Usage:\n\
  \  hx-deploy-tool le-get-certs\n\
  \\n\
  \The config file is read from the file specified with HX_LETSENCRYPT_CONFIG.\n\
  \It defaults to ../etc/letsencrypt-aws.json (relative to the executable).\n\
  \"


helpText :: T.Text
helpText = "\
  \hx-deploy-tool is a simple tool to manage docker based application\n\
  \deployments at helix.\n\
  \\n\
  \A software release is a (typically small) zip archive containing\n\
  \pure configuration, typically a docker-compose file and other\n\
  \configuration files templates. `hx-deploy-tool` can fetch such\n\
  \releases from AWS S3, unpack them, configure it with appropriate\n\
  \environmental details and start the system.\n\
  \ \n\
  \The hx-deploy-tool itself has a json formatted configuration file\n\
  \with a schema specfied in ADL (see config.adl in the source\n\
  \distribution). This specifies:\n\
  \\n\
  \* the local directories where releases are unpacked,\n\
  \* the AWS S3 location where release archives are installed.\n\
  \* the environment information available to configure packages\n\
  \\n\
  \See the source repository (https://github.com/helix-collective/hx-deploy-tool)\n\
  \for details.\n\
  \\n\
  \The following subcommands are available:\n\
  \\n\
  \# hx-deploy-tool help\n\
  \\n\
  \Shows this help text.\n\
  \\n\
  \# hx-deploy-tool fetch context [--retry]\n\
  \\n\
  \Downloads the environmental information files from AWS S3. The\n\
  \`--retry` option is useful during system bootstrap, and it is necessary\n\
  \to wait for the information files to be created.\n\
  \\n\
  \# hx-deploy-tool list-releases\n\
  \\n\
  \Shows the release archives available in S3, most recent first.\n\
  \\n\
  \# hx-deploy-tool unpack <release> <todir>\n\
  \\n\
  \Unpack and configure the specified  release into the given directory.\n\
  \\n\
  \# hx-deploy-tool aws-docker-login-cmd\n\
  \\n\
  \Assuming this is run on a AWS EC2 instance, this subcommand runs\n\
  \the appropriate docker login command using the instance profile\n\
  \to permit access to configured ECR repositories. This command is\n\
  \typically run on first boot.\n\
  \\n\
  \# hx-deploy-tool select <release>\n\
  \\n\
  \The command combines all of the necessary functions to replace\n\
  \any existing running release with the specified release.\n\
  \Specifically it:\n\
  \\n\
  \ - Fetches the release archive from S3\n\
  \ - Unpacks it\n\
  \ - configures it for the local environment\n\
  \ - runs the prestart script. This will typically pull\n\
  \   necessary docker images.\n\
  \ - stops the current release (if any)\n\
  \ - starts the new release\n\
  \ - switches the `current` symlink to point to the new release\n\
  \\n\
  \# hx-deploy-tool show-log\n\
  \\n\
  \Show the history of releases deployed via the select command.\n\
  \\n\
  \# hx-deploy-tool le-get-certs\n\
  \\n\
  \Use the letsencrypt service to obtain or renew SSL certificates\n\
  \\n\
  \"
