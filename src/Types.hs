{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.Text (Text)

-- This needs to be BSON not JSON
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

-- Expression Problem? Or something else
-- + Flesh out these records for each command
data Help

data Init

data Positive

data Negative

data UserTotal

data TeamTotal

data WebhookResponse = WebhookResponse {
  rtext :: String,
  rchannel :: String,
  username :: String
  } deriving (Generic, Show)

instance ToJSON WebhookResponse where
  toJSON (WebhookResponse text channel username) = object
    [ "text" .= text
    , "channel" .= channel
    , "username" .= username
    ]
