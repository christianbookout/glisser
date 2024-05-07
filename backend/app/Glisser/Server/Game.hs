module Glisser.Server.Game (GameState(..), handleCommand, defaultGameState) where

import Control.Concurrent.STM
import Glisser.Server.Protocol (ServerCommand(..))
import Glisser.Types ( Board(..), Team(..))
import Data.Maybe (fromJust)
import Glisser.Glisser (makeMove)

-- Each game state will be unique to the game type. The `GameState` type variable allows this.

data GameState = GameState
    { board :: Board
    , team  :: Team
    , playersTurn :: Bool
    }

defaultGameState :: GameState
defaultGameState = GameState
    { board = Board []
    , team = A
    , playersTurn = False
    }

handleCommand :: ServerCommand -> TVar GameState -> STM ()
handleCommand (SetBoard b) s =
    modifyTVar s (\gs -> gs { board = b })
handleCommand (MakeMove m) s = 
    modifyTVar s (\gs -> gs {board = fromJust (makeMove (board gs) m)})
handleCommand TurnStart _ = undefined
handleCommand _ _ = undefined