module Glisser.Read where

import Glisser.Types

import Text.Parsec
import Text.Parsec.String (Parser)
    
team :: Parser Team
team = (char 'a' >> return A)
   <|> (char 'b' >> return B)
   <|> (char 'c' >> return C)
   <|> (char 'd' >> return D)

piece :: Parser GameObject
piece = do
    t <- team
    char 'P'
    return $ Piece t

goal :: Parser GameObject
goal = do
    t <- team
    char 'G'
    return $ Goal t []

block :: Parser GameObject
block = do
    t <- team
    char 'B'
    return $ Block t

emptySpaces :: Parser [GameObject]
emptySpaces = do
    n <- many1 digit
    return $ replicate (read n) Empty

gameObject :: Parser GameObject
gameObject = try piece <|> try goal <|> try block

gameObjects :: Parser [GameObject]
gameObjects = do
    obj <- gameObject
    spaces <- option [] emptySpaces
    return $ obj : spaces

gameRow :: Parser [GameObject]
gameRow = concat <$> many gameObjects

board :: Parser Board
board = many gameRow <* eof