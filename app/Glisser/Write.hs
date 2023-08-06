module Glisser.Write (Serializable(..)) where

import Glisser.Types

import Data.List (group)

-- | A class for serializing Glisser types to a string.
class Serializable a where
    -- | Convert a type to a Glisser string based on the protocol spec.
    serialize :: a -> String

instance Serializable Move where
    serialize (Move (x, y) dir) = show x ++ "," ++ show y ++ serialize dir

instance Serializable Board where
    serialize (Board board) =
        concatMap (\row ->
              concatMap processEmpties (group row) ++ "/"
        ) board
      where
        processEmpties g@(GLEmpty:_) = show $ length g
        processEmpties g = concatMap serialize g

instance Serializable GameObject where
    serialize obj = case obj of
        GLPiece team -> "P" ++ serialize team
        GLGoal dir team -> "G" ++ serialize dir ++ serialize team
        GLBlock team -> "B" ++ serialize team
        GLEmpty -> "1" -- Default to showing 1. Shouldn't happen.

instance Serializable Team where
    serialize team = case team of
        A -> "A"
        B -> "B"
        C -> "C"
        D -> "D"

instance Serializable Direction where
    serialize dir = case dir of
        DirUp -> "U"
        DirDown -> "D"
        DirLeft -> "L"
        DirRight -> "R"

