module Glisser.Glisser (makeMove) where
import Glisser.Types

import Control.Monad (guard)

type Glisser a = Maybe a

getPiece :: Board -> (Int, Int) -> Glisser GameObject
getPiece (Board rows) (x, y) = do
  guard $ y < length rows
  let row = rows !! y
  guard $ x < length row
  return $ row !! x

makeMove :: Board -> Move -> Glisser Board
makeMove board (x, y, dir) = return board