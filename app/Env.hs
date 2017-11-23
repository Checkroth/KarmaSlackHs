{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Env where

import Data.Text (Text, pack)
import System.Environment
import qualified Configuration.Dotenv as Dotenv

data EnvVars = EnvVars
  { portNum :: Integer
  , dbName :: Text
  , dbEndpoint :: String
  , dbUsr :: Text
  , dbPass :: Text
  , serverPort :: Int
  } deriving(Show)

getEnvVars :: String -> IO EnvVars
getEnvVars file = do
  Dotenv.loadFile False file
  port <- getEnv "MONGODB_PORT"
  let portNum = read port :: Integer
  dbName <- fmap pack $ getEnv "MONGODB_NAME"
  dbEndpoint <- getEnv "MONGODB_ENDPOINT"
  dbUsr <- fmap pack $ getEnv "MONGODB_USERNAME"
  dbPass <- fmap pack $ getEnv "MONGODB_PASSWORD"
  serverPortEnv <- getEnv "PORT"
  let serverPort = read serverPortEnv :: Int
  return $ EnvVars portNum dbName dbEndpoint dbUsr dbPass serverPort

