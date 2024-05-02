{-# LANGUAGE InstanceSigs #-}
module Glisser.Server.Protocol
    ( ServerCommand (..)
    , ResponseCode (..)
    , ClientCommand (..)
    ) where
import Glisser.Types as Gl ( Board, Move, Team )
import Glisser.Write (serialize)
import Glisser.Read ( parseBoard, parseMove, readTeam )
import Text.Read (readMaybe)

data ResponseCode
    = Success
    | MoveFailed
    deriving (Show, Eq, Enum)

data ClientCommand
    = RequestTeam !Team
    | Ping
    | SEND_MOVE !Move
    | Forfeit

instance Show ClientCommand where
    show (RequestTeam team) = "REQUEST_TEAM " ++ serialize team
    show Ping = "PING"
    show (SEND_MOVE move) = "SEND_MOVE " ++ serialize move
    show Forfeit = "FORFEIT"

instance Read ClientCommand where
    readsPrec :: Int -> ReadS ClientCommand
    readsPrec _ str = case command of
        Just cmd -> [(cmd, "")]
        _ -> []
        where command = case words str of
                ["REQUEST_TEAM", teamStr] -> RequestTeam <$> readTeam teamStr
                ["PING"] -> Just Ping
                ["SEND_MOVE", moveStr] -> SEND_MOVE <$> parseMove moveStr
                ["FORFEIT"] -> Just Forfeit
                _ -> Nothing

data ServerCommand =
    SetBoard !Board
  | SetTeam !Team
  | TurnStart
  | GameEnd !Team
  | Response ResponseCode
  | MakeMove !Move

instance Show ServerCommand where
    show (SetBoard board) = "SET_BOARD " ++ serialize board
    show (SetTeam team)   = "SET_TEAM "  ++ serialize team
    show TurnStart        = "TURN_START"
    show (GameEnd team)   = "GAME_END "  ++ serialize team
    show (Response code)  = "RESPONSE "  ++ show (fromEnum code)
    show (MakeMove move)  = "MAKE_MOVE " ++ serialize move

instance Read ServerCommand where
    readsPrec :: Int -> ReadS ServerCommand
    readsPrec _ str = case command of
        Just cmd -> [(cmd, "")]
        _ -> []
        where command = case words str of
                ["SET_BOARD", boardStr] -> SetBoard <$> parseBoard boardStr
                ["SET_TEAM", teamStr] -> SetTeam <$> readTeam teamStr
                ["TURN_START"] -> Just TurnStart
                ["RESPONSE", codeStr] -> case toEnum <$> (readMaybe codeStr :: Maybe Int) of
                    Just code -> Just $ Response code
                    Nothing -> Nothing
                ["GAME_END", teamStr] -> GameEnd <$> readTeam teamStr
                ["MAKE_MOVE", moveStr] -> MakeMove <$> parseMove moveStr
                _ -> Nothing

