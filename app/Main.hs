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
import qualified Configuration.Dotenv as Dotenv

--server :: Pipe -> Server Routes -- Cleaner typing style for later refactoring
server :: Pipe -> IncomingRequest -> Handler WebhookResponse
server pipe req =
  return res
  where
    res = WebhookResponse "test" "somechannel" "someuser"
{-  mongoWrite command
  return $ buildResult command
  where
    command = parseCommand req
-}
parseCommand :: IncomingRequest -> SlackCommand
parseCommand arg = Help

mongoWrite :: SlackCommand -> IO ()
mongoWrite Help = return ()
mongoWrite Init = return ()
mongoWrite (Positive amount username teamname) = return ()
mongoWrite (Negative amount username teamname) = return ()
mongoWrite (UserTotal username teamname) = return ()
mongoWrite (TeamTotal teamname) = return ()

app :: Pipe -> Application
app pipe = serve karmaApi $ server pipe

main :: IO ()
main = do
  EnvVars{..} <- getEnvVars ".env"
  pipe <- connect (Host dbEndpoint (PortNumber (fromInteger portNum)))
  authenticated <- access pipe master dbName $ auth dbUsr dbPass
  run serverPort $ app pipe
