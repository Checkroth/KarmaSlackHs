{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Types where

import Data.Aeson.Types
import Data.Aeson (ToJSON)
import GHC.Generics
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Database.MongoDB (Action)

type MongoExec m a = MonadIO m => Action m a -> m a

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

data SlackCommand = Help String
                  | Init -- Init is a different endpoint I think
                  | Positive Int String String
                  | Negative Int String String
                  | UserTotal String String
                  | TeamTotal String
                  | Invalid

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
