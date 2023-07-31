module Glisser.Types (Team(..), Direction(..), GameObject(..), Board(..)) where

data Team = A | B | C | D

data Direction = DirUp | DirDown | DirLeft | DirRight

data GameObject
    = Piece Team
    -- ^ Game piece and team it belongs to
    | Goal Team Direction
    -- ^ Goal and team who wants to score inside of it, as well as the direction the goal faces
    | Block Team
    -- ^ Block piece and team it belongs to
    | Empty

newtype Board = Board [[GameObject]]

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

instance Show Board where
    show (Board board) = ""