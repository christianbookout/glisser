{-# LANGUAGE InstanceSigs #-}
module Glisser.Client.Protocol
    ( Command (..)
    , ErrorCode (..)
    ) where
import Glisser.Types as Gl ( Board, Move, Team )
import Glisser.Write (serialize)
import Glisser.Read ( parseBoard, parseMove, readTeam )

data ErrorCode =
    MoveFailed
    deriving (Show, Eq, Enum)

data Command =
    SetBoard !Board
  | MakeMove !Move
  | TurnStart
  | Error !ErrorCode
  | GameEnd !Team
  | JoinGame
  | LeaveGame
  | Forfeit

instance Show Command where
    show (SetBoard board) = "SET_BOARD " ++ serialize board
    show (MakeMove move) = "MAKE_MOVE " ++ serialize move
    show TurnStart = "TURN_START"
    show (Error code) = "ERROR " ++ show (fromEnum code)
    show (GameEnd team) = "GAME_END " ++ serialize team
    show JoinGame = "JOIN_GAME"
    show LeaveGame = "LEAVE_GAME"
    show Forfeit = "FORFEIT"

instance Read Command where
    readsPrec :: Int -> ReadS Command
    readsPrec _ str = case command of
        Just cmd -> [(cmd, "")]
        _ -> []
        where command = case words str of
                ["SET_BOARD", boardStr] -> SetBoard <$> parseBoard boardStr
                ["MAKE_MOVE", moveStr] -> MakeMove <$> parseMove moveStr
                ["TURN_START"] -> Just TurnStart
                -- ["ERROR", codeStr] -> case readErrorCode codeStr of
                --     Just code -> Just $ Error code
                --     Nothing -> Nothing
                ["GAME_END", teamStr] -> GameEnd <$> readTeam teamStr
                ["JOIN_GAME"] -> Just JoinGame
                ["LEAVE_GAME"] -> Just LeaveGame
                ["FORFEIT"] -> Just Forfeit
                _ -> Nothing

