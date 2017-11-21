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

karmas :: [Karma]
karmas =
  [ Karma "teamone" "userone" 100
  , Karma "teamtwo" "karmatwo" 1
  ]

server :: Pipe -> Server Routes
server pipe hoge = return karmas

parseCommand :: String -> SlackCommand
parseCommand arg = Help

runCommand :: SlackCommand -> IO ()
runCommand Help = return ()
runCommand Init = return ()
runCommand (Positive amount username teamname) = return ()
runCommand (Negative amount username teamname) = return ()
runCommand (UserTotal username teamname) = return ()
runCommand (TeamTotal teamname) = return ()

app :: Pipe -> Application
app pipe = serve karmaApi $ server pipe

main :: IO ()
main = do
  EnvVars{..} <- getEnvVars ".env"
  pipe <- connect (Host dbEndpoint (PortNumber 26762)) -- Issue with Num vs Int etc.
  authenticated <- access pipe master dbName $ auth dbUsr dbPass
  run 8081 $ app pipe
