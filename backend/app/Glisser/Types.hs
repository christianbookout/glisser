{-# OPTIONS_GHC -funbox-strict-fields #-}

module Glisser.Types
  ( Team(..)
  , Direction(..)
  , GameObject(..)
  , Board(..)
  , Move(..)
  , Vector2
  , addPos
  , Glisser
  , isPiece
  , isGoal
  , isBlock
  , isEmpty
  , getDirection
  , getChange
  , oppositeDir
  ) where

-- | A team to represent a player or game piece.
data Team = A | B | C | D deriving (Eq, Show)

-- | A direction to represent movement or facing.
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show)

-- | A game object representing a square of the board.
data GameObject
    = GLPiece !Team -- ^ Game piece and team it belongs to
    | GLGoal !Direction !Team
    -- ^ Goal and team who wants to score inside of it, as well as the direction
    -- the goal faces
    | GLBlock !Team -- ^ Block piece and team it belongs to
    | GLEmpty
    deriving (Eq, Show)

isPiece :: GameObject -> Bool
isPiece (GLPiece _) = True
isPiece _ = False

isGoal :: GameObject -> Bool
isGoal (GLGoal _ _) = True
isGoal _ = False

isBlock :: GameObject -> Bool
isBlock (GLBlock _) = True
isBlock _ = False

isEmpty :: GameObject -> Bool
isEmpty GLEmpty = True
isEmpty _ = False

-- | A board is a 2D array of game objects.
newtype Board = Board [[GameObject]] deriving Show

-- | A move is a position and a direction.
data Move = Move !Vector2 !Direction deriving Show

-- | A vector2 is a tuple of two integers.
type Vector2 = (Int, Int)

-- | Convert a direction type to a direction vector.
getChange :: Direction -> Vector2
getChange DirUp = (0, -1)
getChange DirDown = (0, 1)
getChange DirLeft = (-1, 0)
getChange DirRight = (1, 0)

-- | Convert a vector to a direction type. Does not support diagonals.
getDirection :: Vector2 -> Direction
getDirection (x, y)
  | x == 0 && y > 0 = DirUp
  | x == 0 && y < 0 = DirDown
  | x > y && y == 0 = DirLeft
  | otherwise = DirRight

oppositeDir :: Direction -> Direction
oppositeDir DirUp = DirDown
oppositeDir DirDown = DirUp
oppositeDir DirLeft = DirRight
oppositeDir DirRight = DirLeft

addPos :: Vector2 -> Vector2 -> Vector2
addPos (x1, y1) (x2, y2) =  (x1 + x2, y1 + y2)

type Glisser a = Maybe a
