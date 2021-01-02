{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Info
       (info)
where

import qualified Data.ByteString.Lazy      as LB
import           Network.HTTP.Types        (methodGet, status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai

info :: LB.ByteString -> Middleware
info payload app req sendResponse =
  case rawPathInfo req of
    "/info" ->
      if getReq
        then returnInfo
        else next
    _ -> next
  where
    next = app req sendResponse
    getReq = requestMethod req == methodGet
    returnInfo = sendResponse $ responseLBS status200 headers payload
    headers = [(hContentType, "application/json")]
