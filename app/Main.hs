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
  liftIO $ handler req

handler :: IncomingRequest -> IO WebhookResponse
handler req = do
  mongoWrite command
  res <- readResult command
  return $ WebhookResponse res (channel_name(req)) (user_name(req))
  where
    command = parseCommand (words $ text(req)) (channel_name(req))

parseCommand :: [String] -> String -> SlackCommand
parseCommand [_, "help"] _ = Help
parseCommand [_, "!all"] team = TeamTotal team
parseCommand [_, target, pos@('+':_)] team =
  Positive count team target
  where
    count = length $ takeWhile (== '+') pos
parseCommand [_, target, neg@('-':_)] team =
  Negative count team target
  where
    count = length $ takeWhile (== '-') neg
parseCommand [_, target] team = UserTotal target team
parseCommand _ _ = Invalid

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
