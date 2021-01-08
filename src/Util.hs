{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Util
where

import qualified Data.ByteString.Lazy as L (ByteString)
import           Network.HTTP.Types   (hContentType)
import           RIO
import           Servant

errText :: ServerError -> L.ByteString -> ServerError
errText e t = e { errHeaders = [ (hContentType, "text/plain; charset=utf-8") ]
                , errBody    = t }

-- | Creates and throws a simple text/plain ServerError.
throwErrText :: MonadThrow u => ServerError -> L.ByteString -> u a
throwErrText e t = throwM $ errText e t

throwUnauthorized :: MonadThrow u => u a
throwUnauthorized = throwM $ errText err401 "Unauthorized access!"
