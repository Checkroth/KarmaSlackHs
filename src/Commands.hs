{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Types
import Database.MongoDB
import Data.Text (Text)
import Data.Maybe
import qualified Data.Bson

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

readResult :: SlackCommand -> MongoExec m a -> IO (Maybe String)
readResult (Help trigger) _ =
  return $ Just helpMessage
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
readResult Init _ = return $ Just "Init functionality not implemented!"

-- THIS IS WIP SECTION: figuring out how to extract userRecord from 'm',
-- record from m Maybe Document
-- Value from m Maybe Value (Bson.lookup field document :: type)
-- Then return to Main to get operable return value
readResult (Positive amount username teamname) pipe = do
--  userRecord <- pipe $ findOne (select ["userId" =: username] "karmas")
--  let x = fmap (valueAt "teamId") userRecord
--  uid <- fmap (Data.Bson.lookup "userId") userRecord :: Maybe (Maybe String)
--  uid
  return $ Just "nothing"

--readResult Negative
readResult _ _ = return $ Just "Not implemented"
