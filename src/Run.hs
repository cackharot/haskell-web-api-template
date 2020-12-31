{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (runApi) where

import           Import

import           Network.HTTP.Types                   (status200)
import           Network.HTTP.Types.Header            (hContentType)
import           Network.Wai                          (Application, responseLBS)
import           Network.Wai.Handler.Warp             (runEnv)
import qualified Network.Wai.Middleware.Prometheus    as P
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Prometheus                           as P
import qualified Prometheus.Metric.GHC                as P
import           System.Environment                   (lookupEnv)

app :: Application
app req f =
    f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

appMiddlewares :: Bool -> Application -> Application
appMiddlewares isProdEnv = logType . (P.prometheus P.def)
  where
    logType = if isProdEnv then logStdout else logStdoutDev

runApi :: RIO App ()
runApi = do
  hSetBuffering stdin LineBuffering
  let port = 3000 :: Int
  logInfo $ "Started application on " <> (fromString $ show port)
  _ <- P.register P.ghcMetrics
  isProdEnv <- liftIO $ do
    isProdEnv <- maybe False (== "PRODUCTION") <$> lookupEnv "APP_ENVIRONMENT"
    return isProdEnv
  liftIO $ runEnv port $ appMiddlewares isProdEnv $ app
