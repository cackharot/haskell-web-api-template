{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Chakra (
  module X
, module Chakra
, RIO
) where

import           Control.Monad.Trans.Except                (ExceptT (..))
import           Data.Aeson                                as X
import           Data.Default
import           Data.Proxy                                as X
import           Logging                                   as X
import           Network.Wai                               (Middleware)
import           Network.Wai.Cli
import           Network.Wai.Middleware.Health             (health)
import           Network.Wai.Middleware.Info               (info)
import qualified Network.Wai.Middleware.Prometheus         as P
import           Network.Wai.Middleware.RequestLogger      (OutputFormat (..),
                                                            mkRequestLogger,
                                                            outputFormat)
import           Network.Wai.Middleware.RequestLogger.JSON
import qualified Prometheus                                as P
import qualified Prometheus.Metric.GHC                     as P
import           RIO
import           Servant                                   as X hiding (And,
                                                                 Handler)
import qualified Servant

runChakraHandler :: a -> RIO a h -> Servant.Handler h
runChakraHandler ctx a = Servant.Handler $ ExceptT $ try $ runReaderT (unRIO a) ctx

-- Setup servant with custom context so that the handers can take custom effects/ctx
chakraApp ::
     forall β χ ψ.
     ( HasServer χ ψ
     , HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters
     )
  => Proxy χ
  -> Context ψ
  -> β
  -> ServerT χ (RIO β)
  -> Application
chakraApp api sctx ctx actions = serveWithContext api sctx $ srv ctx
  where srv c = hoistServerWithContext api (Proxy @ψ) (runChakraHandler c) actions

runChakraApp ::
     (MonadIO m, HasServer χ '[])
  => (Application -> Application)
  -> β
  -> Proxy χ
  -> ServerT χ (RIO β)
  -> m ()
runChakraApp middlewares ctx api apiHandlers =
  liftIO $
  defWaiMain $ middlewares $ chakraApp api EmptyContext ctx $ apiHandlers

runChakraAppWithMetrics ::
     (MonadIO m, HasServer χ '[])
  => (Application -> Application)
  -> β
  -> Proxy χ
  -> ServerT χ (RIO β)
  -> m ()
runChakraAppWithMetrics middlewares ctx api apiHandlers = do
  _ <- registerMetrics
  runChakraApp middlewares ctx api apiHandlers

chakraMiddlewares :: ToJSON a => a -> IO Middleware
chakraMiddlewares infoDetail = do
  logger <- jsonRequestLogger
  return $ logger . P.prometheus P.def . health . info jsonInfoDetail
  where
    jsonInfoDetail = encode infoDetail

registerMetrics :: MonadIO m => m P.GHCMetrics
registerMetrics = P.register P.ghcMetrics

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
  mkRequestLogger $
  def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
