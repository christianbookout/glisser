{-# LANGUAGE DeriveGeneric #-}
module Glisser.WebServer.Game (
) where

import GHC.Generics (Generic)
import Glisser.WebServer.Player (Player)
import Data.Text (Text)
import Glisser.Types (Board)

data Message = Message
  { messageContent :: Text
  , author :: Player
  } deriving (Generic, Show)

type Chat = [Message]

data Game = Game
  { gameid :: Int
  , players :: [Player]
  , board :: Board
  , turn :: Player
  , winner :: Maybe Player
  , chat :: Chat
  } deriving (Generic, Show)