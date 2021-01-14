{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Util
where

import           Data.Aeson
import qualified Data.ByteString.Lazy     as L (ByteString)
import           Data.Has
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import           Network.HTTP.Types       (hContentType)
import           Network.Wai
import           RIO
import           Servant

errText :: ServerError -> L.ByteString -> ServerError
errText e t =
  e {errHeaders = [(hContentType, "text/plain; charset=utf-8")], errBody = t}

-- | Creates and throws a simple text/plain ServerError.
throwErrText :: MonadThrow u => ServerError -> L.ByteString -> u a
throwErrText e t = throwM $ errText e t

throwUnauthorized :: MonadThrow u => u a
throwUnauthorized = throwM $ errText err401 "Unauthorized access!"

jsonErrorFormatter :: ErrorFormatter
jsonErrorFormatter _tr _req err =
  err400 {errBody = encode err, errHeaders = [("Content-Type", "application/json; charset=utf-8")]}

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404
    { errBody = encode b
    , errHeaders = [("Content-Type", "application/json; charset=utf-8")]
    }
  where
    dl = decodeUtf8With lenientDecode
    b =
      object
        [ "error_code" .= dl "404"
        , "error_message" .= dl "NotFound"
        , "path" .= dl (rawPathInfo req)
        ]

-- | Gets a value of any type from the context.
askObj :: (Has β α, MonadReader α μ) => μ β
askObj = asks getter

-- | Gets a thing from a value of any type from the context. (Useful for configuration fields.)
askOpt :: (Has β α, MonadReader α μ) => (β -> ψ) -> μ ψ
askOpt f = asks $ f . getter
