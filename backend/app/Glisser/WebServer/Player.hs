{-# LANGUAGE DeriveGeneric #-}
    
module Glisser.WebServer.Player (
    Player(..)
) where
import Data.Text (Text)
import GHC.Generics (Generic)

data Player = Player
  { playerid :: Int
  , name :: Text
  , rating :: Int
  } deriving (Generic, Show)
