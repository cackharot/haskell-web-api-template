{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Run (runApi) where

import qualified Domain.API.User                      as U
import           Import

import           Data.Aeson                           (encode)
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (defaultSettings,
                                                       runSettings,
                                                       setBeforeMainLoop,
                                                       setPort)
import           Network.Wai.Middleware.Health        (health)
import           Network.Wai.Middleware.Info          (info)
import qualified Network.Wai.Middleware.Prometheus    as P
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import qualified Prometheus                           as P
import qualified Prometheus.Metric.GHC                as P
import           Servant
import           System.Environment                   (lookupEnv)
import           System.IO                            (hPutStrLn, stderr)

type API = U.UserAPI :<|> EmptyAPI

-- type AppM = ReaderT Handler

appAPI:: Proxy API
appAPI = Proxy

appServer =  U.server :<|> emptyServer

app :: Application
app = serve appAPI $ appServer

appMiddlewares :: Bool -> InfoDetail -> Application -> Application
appMiddlewares isProdEnv infoDetail = logType . P.prometheus P.def . health . info jsonInfoDetail
  where
    logType = if isProdEnv then logStdout else logStdoutDev
    jsonInfoDetail = encode infoDetail

runApi :: RIO App ()
runApi = do
  hSetBuffering stdin LineBuffering
  appConfig <- ask
  eport <-
    liftIO $
    fromMaybe 3000 . (>>= readMaybe) <$>
    lookupEnv "PORT"
  userRepo <- liftIO $ U.newRepo
  let settings =
        setPort eport $
        setBeforeMainLoop
          -- (logInfo $ "Started application on " <> (fromString $ show port)) $
          (hPutStrLn stderr $ "Started application on " ++ show eport)
          defaultSettings
  _ <- P.register P.ghcMetrics
  isProdEnv <-
    liftIO $ do maybe False (== "PRODUCTION") <$> lookupEnv "APP_ENVIRONMENT"
  liftIO $ do
    runSettings settings $ appMiddlewares isProdEnv (appInfoDetail appConfig) $ app
