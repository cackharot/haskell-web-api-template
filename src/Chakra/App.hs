{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Chakra.App
  ( module Chakra.App,
  )
where

import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Cli
import Network.Wai.Middleware.Health (health)
import Network.Wai.Middleware.Info (info)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P
import RIO
import Chakra.RequestLogging
import Servant as X hiding (And, Handler)
import qualified Servant
import qualified Chakra.Types as T (InfoDetail (..))
import Chakra.Util

runChakraHandler :: a -> RIO a h -> Servant.Handler h
runChakraHandler ctx a = Servant.Handler $ ExceptT $ try $ runReaderT (unRIO a) ctx

-- Setup servant with custom context so that the handers can take custom effects/ctx
chakraApp ::
  forall β χ ψ.
  ( HasServer χ ψ,
    HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  Context ψ ->
  β ->
  Proxy χ ->
  ServerT χ (RIO β) ->
  Application
chakraApp sctx ctx api app = serveWithContext api sctx $ srv ctx
  where
    srv c = hoistServerWithContext api (Proxy @ψ) (runChakraHandler c) app

runChakraApp ::
  ( MonadIO m,
    HasServer χ ψ,
    HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  Middleware ->
  Context ψ ->
  β ->
  Proxy χ ->
  ServerT χ (RIO β) ->
  m ()
runChakraApp middlewares sctx ctx api apiHandlers =
  liftIO $
    defWaiMain $ middlewares $ chakraApp sctx ctx api apiHandlers

runChakraAppWithMetrics ::
  ( MonadIO m,
    HasServer χ ψ,
    HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  Middleware ->
  Context ψ ->
  β ->
  Proxy χ ->
  ServerT χ (RIO β) ->
  m ()
runChakraAppWithMetrics middlewares sctx ctx api apiHandlers = do
  _ <- registerMetrics
  runChakraApp middlewares sctx ctx api apiHandlers

chakraMiddlewares :: T.InfoDetail -> IO Middleware
chakraMiddlewares infoDetail = do
  logger <-
    jsonRequestLogger (T.appEnvironment infoDetail) (T.appVersion infoDetail)
  return $ logger . P.prometheus P.def . health . info jsonInfoDetail
  where
    jsonInfoDetail = encode infoDetail

registerMetrics :: MonadIO m => m P.GHCMetrics
registerMetrics = P.register P.ghcMetrics

customErrorFormatters :: ErrorFormatters
customErrorFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = jsonErrorFormatter,
      notFoundErrorFormatter = notFoundFormatter
    }
