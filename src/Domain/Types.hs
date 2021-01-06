{-# LANGUAGE DeriveGeneric #-}
module Domain.Types
where

import           Data.Aeson
import           Data.Text
import           RIO

newtype Name =
  Name Text
  deriving (Eq, Show, Generic)

newtype Email =
  Email Text
  deriving (Eq, Show, Generic)

data User =
  User
    { _name :: !Name
    , _email :: !Email
    }
  deriving (Eq, Show, Generic)

instance ToJSON Name
instance ToJSON Email
instance ToJSON User
