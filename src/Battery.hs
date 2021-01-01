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

infoServer :: InfoDetail -> Server InfoAPI
infoServer d = return d

healthServer :: Server HealthAPI
healthServer = return "Healthy"

batteryServer d = infoServer d :<|> healthServer
