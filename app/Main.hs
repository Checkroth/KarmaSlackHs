{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Text (pack)
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import Database.MongoDB
import Types
import Api
import Env
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Configuration.Dotenv as Dotenv

--server :: Pipe -> Server Routes -- Cleaner typing style for later refactoring
server :: Pipe -> IncomingRequest -> Handler WebhookResponse
server pipe req =
  liftIO $ dothing req

dothing :: IncomingRequest -> IO WebhookResponse
dothing req = do
  mongoWrite command
  res <- readResult command
  return $ WebhookResponse res (channel_name(req)) (user_name(req))
  where
    command = parseRequest req

doio :: SlackCommand -> IO String
doio cmd = return "somestring"

parseRequest :: IncomingRequest -> SlackCommand
parseRequest req =  parseCommand (words $ text(req)) (channel_name(req)) (user_name(req))

parseCommand :: [String] -> String -> String -> SlackCommand
parseCommand [_, "help"] _ _ = Help
parseCommand [_, "!all"] team _ = TeamTotal team
-- guard or pattern match for string containing all one character
--parseCommand [_, target, opscount] team user =
--  | "+":_Positive 1 team user
parseCommand _ _ _ = Invalid

mongoWrite :: SlackCommand -> IO ()
mongoWrite Help = return ()
mongoWrite Init = return ()
mongoWrite (Positive amount username teamname) = return ()
mongoWrite (Negative amount username teamname) = return ()
mongoWrite (UserTotal username teamname) = return ()
mongoWrite (TeamTotal teamname) = return ()

readResult :: SlackCommand -> IO String
readResult a = return "sometest"

app :: Pipe -> Application
app pipe = serve karmaApi $ server pipe

main :: IO ()
main = do
  EnvVars{..} <- getEnvVars ".env"
  pipe <- connect (Host dbEndpoint (PortNumber (fromInteger portNum)))
  authenticated <- access pipe master dbName $ auth dbUsr dbPass
  run serverPort $ app pipe
