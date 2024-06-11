import Test.Tasty (TestTree, testGroup, defaultMain)
import Glisser.Read
import Glisser.Types
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

testTeams ::[TestTree]
testTeams =
  [ testCase "Team A Test" $
      readTeam "A" @?= Just A,
    testCase "Team B Test" $
      readTeam "B" @?= Just B,
    testCase "Team C Test" $
      readTeam "C" @?= Just C,
    testCase "Team D Test" $
      readTeam "D" @?= Just D
  ]

testBoards :: [TestTree]
testBoards =
  [ testCase "Default Board" $
      parseBoard testBoardString @?= Just expectedBoard
  , testCase "Empty Board" $
      parseBoard "" @?= Just (Board [])
  , testCase "Board with one empty row" $
      parseBoard "1" @?= Just (Board [[]])
  , testCase "Board with one row" $
        parseBoard "1P" @?= Just (Board [[GLPiece A]])
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $
  [ testCase "Direction Parsing Test" $
      parseMove "1,1U" @?= Just (Move (1, 1) DirUp)
    -- testCase "Board Parsing Test" $
    --   parseBoard testBoardString @?= Just expectedBoard
  ] ++ testTeams

testBoardString :: String
testBoardString = "1Pa1Pa1PaGDdGDdPa1Pa1Pa1/Pb4Ba2Ba4Pc/14/Pb4Bd2Bd4Pc/14/PbBb1Bc6Bb1BcPc/GRc12GLb/PbBb1Bc6Bb1BcP/Pb4Ba2Ba4Pc/14/Pb4Bd2Bd4Pc/1Pd1Pd1PdGDaGDaPd1Pd1Pd1"

expectedBoard :: Board
expectedBoard = Board
  [ [GLEmpty, GLPiece A, GLEmpty, GLPiece A, GLEmpty, GLPiece A, GLGoal DirDown C, GLGoal DirDown C, GLPiece A, GLEmpty, GLPiece A, GLEmpty, GLPiece A, GLEmpty]
  , [GLPiece B, GLEmpty, GLEmpty, GLEmpty, GLPiece B, GLBlock A, GLEmpty, GLEmpty, GLPiece B, GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLPiece C]
  , replicate 14 GLEmpty
  , [GLPiece B, GLEmpty, GLEmpty, GLEmpty, GLBlock D, GLEmpty, GLEmpty, GLBlock D, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLPiece C]
  , replicate 14 GLEmpty
  , [GLPiece B, GLBlock B, GLEmpty, GLBlock C, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLBlock B, GLEmpty, GLBlock C, GLPiece C]
  , [GLGoal DirRight C, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLGoal DirLeft B]
  , [GLPiece B, GLBlock B, GLEmpty, GLBlock C, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLBlock B, GLEmpty, GLBlock C, GLPiece C]
  , [GLPiece B, GLEmpty, GLEmpty, GLEmpty, GLPiece B, GLBlock A, GLEmpty, GLEmpty, GLPiece B, GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLPiece C]
  , replicate 14 GLEmpty
  , [GLPiece B, GLEmpty, GLEmpty, GLEmpty, GLBlock D, GLEmpty, GLEmpty, GLBlock D, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLPiece C]
  , replicate 14 GLEmpty
  , [GLPiece D, GLEmpty, GLPiece D, GLEmpty, GLPiece D, GLEmpty, GLPiece D, GLGoal DirUp A, GLGoal DirUp A, GLPiece D, GLEmpty, GLPiece D, GLEmpty, GLPiece D]
  ]
