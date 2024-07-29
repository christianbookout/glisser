module Glisser.Read (parseBoard, parseMove, readTeam) where

import Glisser.Types

import Text.Parsec
import Text.Parsec.String (Parser)
    
-- | Parse a team. Either A, B, C, or D. Team character comes after the piece
-- type when parsing a board.
team :: Parser Team
team = (char 'a' >> return A)
   <|> (char 'b' >> return B)
   <|> (char 'c' >> return C)
   <|> (char 'd' >> return D)
   
readTeam :: String -> Maybe Team
readTeam = eitherToMaybe . parse team ""

-- | Parse a direction. Either U, D, L, or R.
direction :: Parser Direction
direction = (char 'U' >> return DirUp)
        <|> (char 'D' >> return DirDown)
        <|> (char 'L' >> return DirLeft)
        <|> (char 'R' >> return DirRight)

-- | Parse a game piece by character P.
piece :: Parser (Team -> GameObject)
piece = do
    _ <- char 'P'
    return GLPiece


-- | Parse a goal by character G followed by a direction.
goal :: Parser (Team -> GameObject)
goal = do
    _ <- char 'G'
    GLGoal <$> direction

-- | Parse a block by character B.
block :: Parser (Team -> GameObject)
block = do
    _ <- char 'B'
    return GLBlock

-- | Parse a game object by parsing a piece, goal, or block followed by a team
-- as all game objects are held by a specific team.
gameObject :: Parser GameObject
gameObject = (try piece <|> try goal <|> try block) <*> try team

-- | Parse a number specifying the number of spaces that are empty.
emptySpaces :: Parser [GameObject]
emptySpaces = try $ do
    n <- many1 digit
    return $ replicate (read n) GLEmpty

-- | Parse a row of a game board. A row is a list of game objects that are
-- either pieces, goals, blocks, or empty spaces. Its length must match the
-- board's size.
gameRow :: Parser [GameObject]
gameRow = concat <$> many ((:[]) <$> gameObject <|> emptySpaces)

board :: Parser Board
board = Board <$> sepBy gameRow (char '/')

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

parseBoard :: String -> Maybe Board
parseBoard = eitherToMaybe . parse board ""

-- | Parse a move by parsing a number specifying the x coordinate, a comma, a
-- number specifying the y coordinate, and a direction.
move :: Parser Move
move = do
    x <- many1 digit
    _ <- char ','
    y <- many1 digit
    Move (read x, read y) <$> direction

parseMove :: String -> Maybe Move
parseMove = eitherToMaybe . parse move ""