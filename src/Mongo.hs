{-# LANGUAGE Rank2Types #-}

module Mongo where

import Types
import Database.MongoDB

--mongoWrite :: SlackCommand -> IO (Maybe String)
mongoWrite Init _ = return Just "dummy"
mongoWrite (Positive amount username teamname) pipe = return Just "dummy"
mongoWrite (Negative amount username teamname) _ = return Just "dummy"

--mongoRead :: SlackCommand -> MongoExec -> IO (Maybe String)
mongoRead (UserTotal username teamname) pipe = return $ Just "UserTotal mongo driver not implemented"
mongoRead (TeamTotal teamname) pipe = return $ Just "Teamtoatl mongo driver not implemented"
