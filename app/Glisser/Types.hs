{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE InstanceSigs #-}

module Glisser.Types
  ( Team(..)
  , Direction(..)
  , GameObject(..)
  , Board(..)
  , GameRow
  , Move
  ) where
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
type GameRow = [GameObject]

-- | A board is a 2D array of game objects.
newtype Board = Board [GameRow]

type Move = (Int, Int, Direction)

-- TODO perhaps these should be moved to a Read module

class ToGlisserFen a where
  -- | Convert a type to a Glisser FEN string
  toGlisserFen :: a -> String

instance ToGlisserFen Team where
    toGlisserFen A = "A"
    toGlisserFen B = "B"
    toGlisserFen C = "C"
    toGlisserFen D = "D"

instance ToGlisserFen Direction where
    toGlisserFen DirUp = "U"
    toGlisserFen DirDown = "D"
    toGlisserFen DirLeft = "L"
    toGlisserFen DirRight = "R"

instance ToGlisserFen GameObject where
    toGlisserFen (GLPiece _) = "P"
    toGlisserFen (GLGoal _ _) = "G"
    toGlisserFen (GLBlock _) = "B"
    toGlisserFen GLEmpty = "E"

instance ToGlisserFen Board where
  toGlisserFen (Board board) =
      concatMap (\row ->
           concatMap processEmpties (group row) ++ "/"
      ) board
    where
      processEmpties g@(GLEmpty:_) = show $ length g
      processEmpties g = concatMap toGlisserFen g