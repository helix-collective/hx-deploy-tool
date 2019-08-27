{-# LANGUAGE OverloadedStrings #-}
module Commands.LetsEncrypt where

import qualified Data.Aeson as JS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as LNE
import qualified Data.Text as T
import qualified Data.Text.IO as T

import ADL.Core
import ADL.Config
import Control.Applicative
import Control.Monad.Trans.AWS
import Control.Monad.IO.Class
import Control.Lens
import Data.Maybe(fromMaybe)
import Data.Monoid
import Network.AWS.Route53
import Network.AWS.S3
import Network.AWS.Data(fromText)
import System.Environment(getArgs, getEnv, getExecutablePath)
import System.Exit(exitWith,ExitCode(..))
import System.FilePath((</>))
import System.Process(callProcess)
import System.IO(stdout)
import Util.Aws(mkAwsEnv0)

-- | Request new or renewed certificates
getCerts :: LetsEncryptConfig -> IO ()
getCerts c = do
  exepath <- getExecutablePath
  let exe = (T.unpack . lec_certbotPath) c
      basedir = (T.unpack . lec_basedir) c
      email = (T.unpack . lec_email) c
      domains = (T.unpack . T.intercalate "," . lec_domains) c
      authhook = exepath <> " le-auth-hook"
      cleanuphook = exepath <> " le-cleanup-hook"
      args =
        [ "--manual"
        , "--config-dir", basedir </> "config"
        , "--work-dir", basedir </> "work"
        , "--logs-dir", basedir </> "logs"
        , "--preferred-challenges", "dns"
        , "--manual-auth-hook", authhook
        , "--manual-cleanup-hook", cleanuphook
        , "--manual-public-ip-logging-ok"
        , "--agree-tos"
        , "-n"
        , "-m", email
        , "-d", domains
        , "certonly"
        ]
  print args
  callProcess exe args

authHook :: LetsEncryptConfig -> IO ()
authHook c = do
  env <- mkAwsEnv1 c
  (certbotDomain,certbotValidation,hostedZone) <- getDnsDetails c
  let batch = changeBatch (pure (change Create rset))
      rset = resourceRecordSet  ("_acme-challenge." <> certbotDomain) Txt
           & rrsTTL .~ Just 60
           & rrsResourceRecords .~ (Just (pure (resourceRecord certbotValidation)))
  runResourceT . runAWST env $ do
    resp <- send (changeResourceRecordSets hostedZone batch)
    let ready = getChange (resp ^. crrsrsChangeInfo ^. ciId)
    await resourceRecordSetsChanged ready
  return ()

cleanupHook :: LetsEncryptConfig -> IO ()
cleanupHook c = do
  env <- mkAwsEnv1 c
  (certbotDomain,certbotValidation,hostedZone) <- getDnsDetails c
  let batch = changeBatch (pure (change Delete rset))
      rset = resourceRecordSet  ("_acme-challenge." <> certbotDomain) Txt
           & rrsTTL .~ Just 60
           & rrsResourceRecords .~ (Just (pure (resourceRecord certbotValidation)))
  runResourceT . runAWST env $ do
    send (changeResourceRecordSets hostedZone batch)
  return ()

getDnsDetails :: LetsEncryptConfig -> IO (T.Text,T.Text,ResourceId)
getDnsDetails c = do
  certbotDomain <- T.pack <$> getEnv "CERTBOT_DOMAIN"
  certbotValidation <- T.pack <$> getEnv "CERTBOT_VALIDATION"
  let hostedZone = case fromText (lec_awsHostedZoneId c) of
        Left e -> error e
        Right hz -> hz
  return (certbotDomain,"\""<>certbotValidation<>"\"",hostedZone)

mkAwsEnv1 :: LetsEncryptConfig -> IO Env
mkAwsEnv1 c = do
  env0 <- mkAwsEnv0
  l <- newLogger loglevel stdout
  return (env0 & envLogger .~ l)
  where
    loglevel = case lec_verbosity c of
      Verbosity_noisy -> Debug
      Verbosity_quiet -> Error
