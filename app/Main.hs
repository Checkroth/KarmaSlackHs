{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import Data.Text (pack)
import Data.Maybe
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import Database.MongoDB
import Types
import Api
import Env
import Commands
import Mongo
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Configuration.Dotenv as Dotenv


--server :: Pipe -> Server Routes -- Cleaner typing style for later refactoring
-- Pipe - Should certainly use some control Monad instead of tossing it around like this

server :: MongoExec m a -> IncomingRequest -> Handler WebhookResponse
server pipe req =
  liftIO $ handler req pipe

handler :: IncomingRequest -> MongoExec m a -> IO WebhookResponse
handler req pipe = do
--  mongoWrite command pipe
  res <- readResult command pipe
--  return $ WebhookResponse res (channel_name(req)) (user_name(req))
  return $ WebhookResponse (fromMaybe "notfound" res) "two" "three"
  where
    command = parseCommand (tail $ words $ text(req)) (channel_name(req))


app :: MongoExec m a -> Application
app pipe = serve karmaApi $ server pipe

main :: IO ()
main = do
  EnvVars{..} <- getEnvVars ".env"
  pipe <- connect (Host dbEndpoint (PortNumber (fromInteger portNum)))
  let exec act = access pipe master dbName act
  exec $ auth dbUsr dbPass
  run serverPort $ app exec
