{-# OPTIONS_GHC -funbox-strict-fields #-}

module Glisser.Types (Team(..), Direction(..), GameObject(..), Board(..), GameRow(..)) where
import Data.List (group)

-- | A team to represent a player or game piece.
data Team = A | B | C | D deriving Eq

-- | A direction to represent movement or facing.
data Direction = DirUp | DirDown | DirLeft | DirRight deriving Eq

-- | A game object representing a square of the board.
data GameObject
    = GLPiece !Team -- ^ Game piece and team it belongs to
    | GLGoal !Direction !Team 
    -- ^ Goal and team who wants to score inside of it, as well as the direction
    -- the goal faces
    | GLBlock !Team -- ^ Block piece and team it belongs to
    | GLEmpty
    deriving Eq

-- | A row of game objects.
newtype GameRow = GameRow [GameObject]

-- | A board is a 2D array of game objects.
newtype Board = Board [GameRow]

instance Show Team where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"

instance Show Direction where
    show DirUp = "U"
    show DirDown = "D"
    show DirLeft = "L"
    show DirRight = "R"

instance Show GameObject where
    show (GLPiece _) = "P"
    show (GLGoal _ _) = "G"
    show (GLBlock _) = "B"
    show GLEmpty = "E"

instance Show GameRow where
  show (GameRow objs) = concatMap processEmpties $ group objs
    where
      processEmpties g@(GLEmpty:_) = show $ length g
      processEmpties g = concatMap show g

instance Show Board where
  show (Board board) = concatMap (\(GameRow row) -> show row ++ "\n") board