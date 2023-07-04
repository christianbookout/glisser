module Game (GameState) where

import Control.Concurrent.STM.TVar (TVar)
import Network.Socket (Socket)

-- We can use a unique integer as the player's ID.
newtype PlayerId = PlayerId Int deriving (Eq, Ord, Show)

-- A Player has an associated id and socket for communication.
data Player = Player
  { playerId    :: PlayerId
  , playerSocket :: Socket
  }

-- Each game state will be unique to the game type. The `GameState` type variable allows this.
data GameState gameState = GameState
  { gameId      :: Int
  , players     :: [Player]
  , gameState   :: TVar gameState
  }