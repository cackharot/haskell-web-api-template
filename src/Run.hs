{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (runApi) where

import           Import

import           Network.HTTP.Types        (status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai               (Application, responseLBS)
import           Network.Wai.Handler.Warp  (runEnv)

app :: Application
app req f =
    f $ responseLBS status200 [(hContentType, "text/plain")] "Hello world!"

runApi :: RIO App ()
runApi = do
  let port = 3000 :: Int
  logInfo $ "Started application on " <> (fromString $ show port)
  liftIO $ runEnv port app
