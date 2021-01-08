{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (runApi) where

import Chakra
import qualified Domain.API.User as U
import Import
import JWT (acquireJwks, getJWTSettings)
import Servant.Auth as SA (JWT)
import Servant.Auth.Server as SAS

type API auths = (SAS.Auth auths AuthenticatedUser :> U.UserAPI) :<|> EmptyAPI

appAPI :: Proxy (API '[SA.JWT])
appAPI = Proxy

appServer = U.server :<|> emptyServer

appMiddlewares :: InfoDetail -> IO Middleware
appMiddlewares = chakraMiddlewares

runApi :: RIO AppConf ()
runApi = do
  hSetBuffering stdin LineBuffering
  appConfig <- ask
  userRepo <- liftIO U.newRepo
  middlewares <- liftIO $ appMiddlewares (appInfoDetail appConfig)
  jwkSet <- liftIO acquireJwks
  myKey <- liftIO generateKey
  let lf = view logFuncL appConfig
      jwtCfg = getJWTSettings myKey jwkSet
      cookieCfg = defaultCookieSettings {cookieIsSecure = SAS.NotSecure}
      sctx = cookieCfg :. jwtCfg :. EmptyContext
  runChakraAppWithMetrics
    middlewares
    sctx
    (lf, appInfoDetail appConfig)
    appAPI
    appServer
