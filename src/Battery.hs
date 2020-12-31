{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Battery
  (BatteryAPI
  , batteryServer)
 where

import           Import
import           Servant

type InfoAPI = "info" :> Get '[JSON] InfoDetail
type HealthAPI= "health" :> Get '[JSON] Text

type BatteryAPI = InfoAPI :<|> HealthAPI

infoServer :: Server InfoAPI
infoServer = return $ InfoDetail "test" "test" "test"

healthServer :: Server HealthAPI
healthServer = return "Healthy"

batteryServer :: Servant.Handler InfoDetail
                       :<|> Servant.Handler Text
batteryServer = infoServer :<|> healthServer


