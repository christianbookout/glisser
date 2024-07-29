module Glisser.Glisser (makeMove) where
import Glisser.Types

import Control.Monad (guard)
import qualified Debug.Trace as Debug

-- | Check if the position is within the board's bounds
isWithinBounds :: Board -> Vector2 -> Bool
isWithinBounds (Board rows) (x, y) =
    y >= 0 && y < length rows && x >= 0 && x < length (rows !! y)

-- | Get the piece at a given Vector2 on the board.
getObject :: Board -> Vector2 -> Glisser GameObject
getObject board@(Board rows) pos@(x, y)  = do
    guard $ isWithinBounds board pos
    let row = rows !! y
    return $ row !! x

-- | Set a piece on the board at a given Vector2 to the given piece.
setObject :: Board -> Vector2 -> GameObject -> Glisser Board
setObject board@(Board rows) pos@(x, y) obj = do
    -- This may be inefficient as we split multiple times. Could use a vector, or splitAt. 
    guard $ isWithinBounds board pos
    let row = rows !! y
    let newRow = take x row ++ [obj] ++ drop (x + 1) row
    return $ Board $ take y rows ++ [newRow] ++ drop (y + 1) rows

-- | Traverse the gane board in a given direction until a non-empty space is
-- found. Return the position of the last empty space. This is only used when a
-- piece is moved.
traverseBoard :: Board -> Team -> Vector2 -> Vector2 -> Glisser Vector2
traverseBoard board team curPos changeVector = do
    let newPos = addPos curPos changeVector
    if isWithinBounds board newPos then do
        newObject <- getObject board newPos
        case newObject of
            GLGoal dir t
                | isOppositeTeam t team
                && dir == oppositeDir (getDirection changeVector) -> return newPos
                | otherwise -> return curPos
            GLEmpty -> traverseBoard board team newPos changeVector
            _ -> return curPos
    else return curPos

-- | Make a move on the board. Return the new board if the move is valid.
makeMove :: Board -> Move -> Glisser Board
makeMove board (Move pos dir) = do
    obj <- Debug.trace "here" $ getObject board pos
    guard (isPiece obj || isBlock obj)  -- Only pieces and blocks can move
    let posChange = getChange dir
    let nextPos = addPos pos posChange
    removedPieceBoard <- setObject board pos GLEmpty
    guard . (\x -> isEmpty x || (isGoal x && isPiece obj)) =<< getObject board nextPos
    newPos <- case obj of
        GLPiece team -> Debug.trace "Traversing" $ traverseBoard board team pos posChange
        _ -> do
            nextObj <- Debug.trace "Checking for empty" getObject board nextPos
            guard $ isEmpty nextObj
            return $ Debug.trace "empty" nextPos
    guard $ newPos /= pos
    newObject <- getObject removedPieceBoard newPos
    -- When the next piece is a goal, then the piece has already been removed
    -- from the board and we're done. Otherwise just set the piece at the new
    -- position to the piece we're moving.
    if isGoal newObject
        then return removedPieceBoard
        else do
            guard $ isEmpty newObject
            setObject removedPieceBoard newPos obj




