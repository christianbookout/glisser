module Glisser.Glisser (makeMove) where
import Glisser.Types

import Control.Monad (guard)

-- | Get the piece at a given Vector2 on the board.
getObject :: Board -> Vector2 -> Glisser GameObject
getObject (Board rows) (x, y)  = do
    guard $ y < length rows
    let row = rows !! y
    guard $ x < length row
    return $ row !! x

-- | Set a piece on the board at a given Vector2 to the given piece.
setObject :: Board -> Vector2 -> GameObject -> Glisser Board
setObject (Board rows) (x, y) obj = do
    -- This may be inefficient as we split multiple times. Could use a vector, or splitAt. 
    guard $ y < length rows
    let row = rows !! y
    guard $ x < length row
    let newRow = take x row ++ [obj] ++ drop (x + 1) row
    return $ Board $ take y rows ++ [newRow] ++ drop (y + 1) rows

-- | Traverse the gane board in a given direction until a non-empty space is
-- found. Return the position of the last empty space. This is only used when a
-- piece is moved.
traverseBoard :: Board -> Vector2 -> Vector2 -> Glisser Vector2
traverseBoard board curPos changeVector = do
    let newPos = addPos curPos changeVector
    newObject <- getObject board newPos
    let canMove = isGoal newObject || isEmpty newObject
    if canMove
        then traverseBoard board newPos changeVector
        else return curPos

-- | Make a move on the board. Return the new board if the move is valid.
makeMove :: Board -> Move -> Glisser Board
makeMove board (Move pos dir) = do
    obj <- getObject board pos
    guard (isPiece obj || isBlock obj)  -- Only pieces and blocks can move
    let posChange = getChange dir
    let nextPos = addPos pos posChange
    nextObject <- getObject board nextPos
    guard (isEmpty nextObject)  -- Ensure there's at least one empty space
    removedPieceBoard <- setObject board pos GLEmpty
    newPos <- if isPiece obj 
              then traverseBoard removedPieceBoard nextPos posChange 
              else return nextPos 
    newObject <- getObject removedPieceBoard newPos
    -- When the next piece is a goal, then the piece has already been removed
    -- from the board and we're done. Otherwise just set the piece at the new
    -- position to the piece we're moving.
    if isGoal newObject
        then return removedPieceBoard
        else setObject removedPieceBoard newPos obj
      



