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
-- Pipe - Should certainly use some control Monad instead of tossing it around like this
server :: Pipe -> IncomingRequest -> Handler WebhookResponse
server pipe req =
  liftIO $ handler req pipe

handler :: IncomingRequest -> Pipe -> IO WebhookResponse
handler req pipe = do
  mongoWrite command pipe
  res <- readResult command
  return $ WebhookResponse res (channel_name(req)) (user_name(req))
  where
    command = parseCommand (tail $ words $ text(req)) (channel_name(req))

parseCommand :: [String] -> String -> SlackCommand
parseCommand ["help"] _ = Help "placeholder"
parseCommand ["!all"] team = TeamTotal team
parseCommand [target@('@':_), pos@('+':_)] team =
  Positive count team target
  where
    count = length $ takeWhile (== '+') pos
parseCommand [target@('@':_), neg@('-':_)] team =
  Negative count team target
  where
    count = length $ takeWhile (== '-') neg
parseCommand [target@('@':_)] team = UserTotal target team
parseCommand _ _ = Invalid

mongoWrite :: SlackCommand -> Pipe -> IO String
mongoWrite (Help trigger) _ = return "dummy"
mongoWrite Init _ = return "dummy"
mongoWrite (Positive amount username teamname) pipe = do
  return "dummy"
mongoWrite (Negative amount username teamname) _ = return "dummy"
mongoWrite (UserTotal username teamname) _ = return "dummy"
mongoWrite (TeamTotal teamname) _ = return "dummy"
mongoWrite Invalid _ = return "dummy"

readResult :: SlackCommand -> IO String
readResult (Help trigger) =
  return helpMessage
  where
    helpMessage = unlines ["How to use karma:",
                           "Positive karma = " ++ trigger ++ ": @user ++",
                           "Negative karma = " ++ trigger ++ ": @user ++",
                           "User karma = " ++ trigger ++ ": @user",
                           "Team karma = " ++ trigger ++ ": !all",
                           "Setup karma = " ++ trigger ++ ": init {",
                           " \"incomingWebhookUrl\": \" https://hooks.slack.com/service/my/incomingwebhook\",",
                           " \"outgoingToken\": \"myoutgoingtokens\"",
                           " }"]

readResult a = return "sometest"

app :: Pipe -> Application
app pipe = serve karmaApi $ server pipe

main :: IO ()
main = do
  EnvVars{..} <- getEnvVars ".env"
  pipe <- connect (Host dbEndpoint (PortNumber (fromInteger portNum)))
  let exec act = access pipe master dbName act
  authenticated <- exec $ auth dbUsr dbPass
  run serverPort $ app pipe
