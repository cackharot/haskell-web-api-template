{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Run (runApi) where

import           Chakra
import qualified Domain.API.User     as U
import           Import
import           JWT                 (getJWTAuthSettings)
import           Servant.Auth        as SA (JWT)
import           Servant.Auth.Server as SAS

type API auths = (SAS.Auth auths AuthenticatedUser :> U.UserAPI) :<|> EmptyAPI

appAPI :: Proxy (API '[SA.JWT])
appAPI = Proxy

appMiddlewares :: InfoDetail -> IO Middleware
appMiddlewares = chakraMiddlewares

runApi :: RIO AppConf ()
runApi = do
  hSetBuffering stdin LineBuffering
  appConfig <- ask
  userRepo <- liftIO U.newInMemRepo
  middlewares <- liftIO $ appMiddlewares (appInfoDetail appConfig)
  jwtCfg <- liftIO getJWTAuthSettings
  let lf = view logFuncL appConfig
      cookieCfg = defaultCookieSettings {cookieIsSecure = SAS.NotSecure}
      sctx = cookieCfg :. jwtCfg :. customErrorFormatters :. EmptyContext
      appServer = U.server :<|> emptyServer
  runChakraAppWithMetrics
    middlewares
    sctx
    (lf, appInfoDetail appConfig, userRepo)
    appAPI
    appServer
