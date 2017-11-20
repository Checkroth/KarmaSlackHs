{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson.Types
import GHC.Generics

data Karma = Karma {
  teamId :: String,
  userId :: String,
  karmaPoints :: Integer
  } deriving (Generic, Show)

instance ToJSON Karma

data IncomingRequest = IncomingRequest {
  token :: String,
  team_id :: String,
  team_domain :: String,
  channel_id :: String,
  channel_name :: String,
  timestamp :: String,
  user_id :: String,
  user_name :: String,
  text :: String,
  trigger_word :: String
  } deriving (Generic, Show)

instance FromJSON IncomingRequest
