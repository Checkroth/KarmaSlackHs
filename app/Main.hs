{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Prelude
import Data.Text
import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.Environment
import Database.MongoDB
import Types
import Api
import qualified Configuration.Dotenv as Dotenv

karmas :: [Karma]
karmas =
  [ Karma "teamone" "userone" 100
  , Karma "teamtwo" "karmatwo" 1
  ]

server :: Pipe -> Server Routes
server pipe hoge = return karmas

app :: Pipe -> Application
app pipe = serve karmaApi $ server pipe

-- Need to move env work outside of main for sure
-- Also find a batter way than `val <- fmap pack $ getEnv env_var`
-- Also to make port an Int (probably in similar fashion to better way of val <- fmap pack
main :: IO ()
main = do
  _ <- Dotenv.loadFile False "../env"
  portNum <- getEnv "MONGODB_PORT"
  dbName <- fmap pack $ getEnv "MONGODB_NAME"
  dbEndpoint <- getEnv "MONGODB_ENDPOINT"
  dbUsr <- fmap pack $ getEnv "MONGODB_USERNAME"
  dbPass <- fmap pack $ getEnv "MONGODB_PASSWORD"
  pipe <- connect (Host dbEndpoint (PortNumber 25762))
  authenticated <- access pipe master dbName $ auth dbUsr dbPass
  run 8081 $ app pipe
