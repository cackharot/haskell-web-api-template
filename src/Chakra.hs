{-# LANGUAGE Trustworthy #-}

module Chakra
  ( module X,
    module Chakra,
  )
where

import Chakra.App as X
import Chakra.Config as X
import Chakra.JWT as X
import Chakra.Logging as X
import Chakra.Types as X
import Chakra.Util as X
import Data.Aeson as X
import Data.Has as X
import Data.Proxy as X
import Network.URI as X
import Network.Wai as X
import Network.Wai.Cli as X
import RIO as X
import Servant as X hiding (And, Handler)

type BasicAppCtx = (ModLogger, InfoDetail)

type BasicApp = RIO BasicAppCtx
