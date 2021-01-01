{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Run (runApi) where

import           Battery
import           Import

import           Network.HTTP.Types                   (status200)
import           Network.HTTP.Types.Header            (hContentType)
import           Network.Wai                          (Application, responseLBS)
import           Network.Wai.Handler.Warp             (defaultSettings,
                                                       runSettings,
                                                       setBeforeMainLoop,
                                                       setPort)
import qualified Network.Wai.Middleware.Prometheus    as P
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Prometheus                           as P
import qualified Prometheus.Metric.GHC                as P
import           Servant
import           System.Environment                   (lookupEnv)
import           System.IO                            (hPutStrLn, stderr)

type API = BatteryAPI
  :<|> "empty" :> EmptyAPI

appAPI:: Proxy API
appAPI = Proxy

appServer infoDetail = (batteryServer infoDetail) :<|> emptyServer

app1 :: InfoDetail -> Application
app1 infoDetail = serve appAPI $ appServer infoDetail

-- app :: Application
-- app req f =
--     f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

appMiddlewares :: Bool -> Application -> Application
appMiddlewares isProdEnv = logType . (P.prometheus P.def)
  where
    logType = if isProdEnv then logStdout else logStdoutDev

runApi :: RIO App ()
runApi = do
  hSetBuffering stdin LineBuffering
  app <- ask
  eport <- liftIO $ fromMaybe (port $ appOptions app ) . (>>= readMaybe) <$> lookupEnv "PORT"
  let settings =
        setPort eport $
        setBeforeMainLoop
          -- (logInfo $ "Started application on " <> (fromString $ show port)) $
          (hPutStrLn stderr $ "Started application on " ++ show eport) $
        defaultSettings
  _ <- P.register P.ghcMetrics
  isProdEnv <-
    liftIO $ do
      isProdEnv <- maybe False (== "PRODUCTION") <$> lookupEnv "APP_ENVIRONMENT"
      return isProdEnv
  liftIO $ runSettings settings $ appMiddlewares isProdEnv $ app1 (appInfoDetail app)
