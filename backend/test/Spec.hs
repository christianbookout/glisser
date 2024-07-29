import Test.Tasty (TestTree, testGroup, defaultMain)
import Glisser.Read
import Glisser.Types
import Test.Tasty.HUnit (testCase, (@?=))
import Glisser.Glisser (makeMove)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

testTeams ::[TestTree]
testTeams =
  [ testCase "Team A Test" $
      readTeam "a" @?= Just A,
    testCase "Team B Test" $
      readTeam "b" @?= Just B,
    testCase "Team C Test" $
      readTeam "c" @?= Just C,
    testCase "Team D Test" $
      readTeam "d" @?= Just D
  ]


testDirections :: [TestTree]
testDirections = 
  [ testCase "Direction Parsing Test 1" $
      parseMove "1,1U" @?= Just (Move (1, 1) DirUp)
  , testCase "Direction Parsing Test 2" $
      parseMove "1,1D" @?= Just (Move (1, 1) DirDown)
  , testCase "Direction Parsing Test 3" $
      parseMove "1,1L" @?= Just (Move (1, 1) DirLeft)
  , testCase "Direction Parsing Test 4" $
      parseMove "1,1R" @?= Just (Move (1, 1) DirRight)
  ]

testBoards :: [TestTree]
testBoards =
  [ testCase "Default Board" $
      parseBoard testBoardString @?= Just expectedBoard
  , testCase "Empty board with one empty row" $
      parseBoard "" @?= Just (Board [[]])
  , testCase "Board with one piece" $
        parseBoard "Pa" @?= Just (Board [[GLPiece A]])
  , testCase "Board with two empty rows" $
      parseBoard "3/5" @?= Just (Board [[GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty]])
  , testCase "Board with one row" $
      parseBoard "1Pa1Pa1PaGDdGDdPa1Pa1Pa1" @?= Just (Board [[GLEmpty, GLPiece A, GLEmpty, GLPiece A, GLEmpty, GLPiece A, GLGoal DirDown D, GLGoal DirDown D, GLPiece A, GLEmpty, GLPiece A, GLEmpty, GLPiece A, GLEmpty]])
  , testCase "Board with two simple rows" $ 
        parseBoard "Pa/Pb" @?= Just (Board [[GLPiece A], [GLPiece B]])
  ]

testMoves :: [TestTree]
testMoves =
  [ testCase "Move Parsing Test 1" $
      makeMove (Board [[GLPiece A, GLEmpty], [GLEmpty, GLEmpty]]) (Move (0, 0) DirDown) @?= Just (Board [[GLEmpty, GLEmpty], [GLPiece A, GLEmpty]])
  , testCase "Move Parsing Test 2" $
      makeMove (Board [[GLPiece A, GLEmpty], [GLEmpty, GLEmpty]]) (Move (0, 0) DirUp) @?= Nothing
  , testCase "Move Parsing Test 3" $
      makeMove (Board [[GLPiece A, GLBlock A], [GLEmpty, GLEmpty]]) (Move (0, 0) DirRight) @?= Nothing
  , testCase "Move Parsing Test 4 (3x3)" $
      makeMove (Board [[GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLPiece A, GLEmpty], [GLEmpty, GLEmpty, GLEmpty]]) (Move (1, 1) DirDown) @?= Just (Board [[GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLPiece A, GLEmpty]])
  , testCase "Move Parsing Test 5 (4x4)" $
      makeMove (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLPiece A, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty]]) (Move (1, 1) DirDown) @?= Just (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLPiece A, GLEmpty, GLEmpty]])
  , testCase "Move Parsing Test 6 (5x5)" $
      makeMove (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLPiece A, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLEmpty]]) (Move (1, 1) DirDown) @?= Just (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLPiece A, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLEmpty]])
  , testCase "Move Parsing Test 7 Into Goal" $
      makeMove (Board [[GLPiece A, GLEmpty], [GLGoal DirUp D, GLEmpty]]) (Move (0, 0) DirDown) @?= Just (Board [[GLEmpty, GLEmpty], [GLGoal DirUp D, GLEmpty]])
  , testCase "Move Parsing Test 8 Into Wrong Dir Goal" $
      makeMove (Board [[GLPiece A, GLEmpty], [GLGoal DirDown D, GLEmpty]]) (Move (0, 0) DirDown) @?= Nothing
  ]

testBlockMoves :: [TestTree]
testBlockMoves =
  [ testCase "Block Move Parsing Test 1" $
      makeMove (Board [[GLBlock A, GLEmpty], [GLEmpty, GLEmpty]]) (Move (0, 0) DirDown) @?= Just (Board [[GLEmpty, GLEmpty], [GLBlock A, GLEmpty]])
  , testCase "Block Move Parsing Test 2" $
      makeMove (Board [[GLBlock A, GLEmpty], [GLEmpty, GLEmpty]]) (Move (0, 0) DirUp) @?= Nothing
  , testCase "Block Move Parsing Test 3" $
      makeMove (Board [[GLBlock A, GLBlock A], [GLEmpty, GLEmpty]]) (Move (0, 0) DirRight) @?= Nothing
  , testCase "Block Move Parsing Test 4 (3x3)" $
      makeMove (Board [[GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty], [GLEmpty, GLEmpty, GLEmpty]]) (Move (1, 1) DirDown) @?= Just (Board [[GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty]])
  , testCase "Block Move Parsing Test 5 (4x4)" $
      makeMove (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty]]) (Move (1, 1) DirDown) @?= Just (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty]])
  , testCase "Block Move Parsing Test 6 (5x5)" $
      makeMove (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLEmpty]]) (Move (1, 1) DirDown) @?= Just (Board [[GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLEmpty, GLEmpty, GLEmpty, GLEmpty], [GLEmpty, GLBlock A, GLEmpty, GLEmpty, GLEmpty]])
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests" $
  testDirections ++ testTeams ++ testBoards ++ testMoves ++ testBlockMoves

testBoardString :: String
testBoardString = "1Pa1Pa1PaGDdGDdPa1Pa1Pa1/Pb4Ba2Ba4Pc/14/Pb4Bd2Bd4Pc/14/PbBb1Bc6Bb1BcPc/GRc12GLb/PbBb1Bc6Bb1BcPc/Pb4Ba2Ba4Pc/14/Pb4Bd2Bd4Pc/1Pd1Pd1PdGDaGDaPd1Pd1Pd1"

expectedBoard :: Board
expectedBoard = Board [[GLEmpty,GLPiece A,GLEmpty,GLPiece A,GLEmpty,GLPiece A,GLGoal DirDown D,GLGoal DirDown D,GLPiece A,GLEmpty,GLPiece A,GLEmpty,GLPiece A,GLEmpty],[GLPiece B,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLBlock A,GLEmpty,GLEmpty,GLBlock A,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLPiece C],[GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty],[GLPiece B,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLBlock D,GLEmpty,GLEmpty,GLBlock D,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLPiece C],[GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty],[GLPiece B,GLBlock B,GLEmpty,GLBlock C,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLBlock B,GLEmpty,GLBlock C,GLPiece C],[GLGoal DirRight C,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLGoal DirLeft B],[GLPiece B,GLBlock B,GLEmpty,GLBlock C,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLBlock B,GLEmpty,GLBlock C,GLPiece C],[GLPiece B,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLBlock A,GLEmpty,GLEmpty,GLBlock A,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLPiece C],[GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLEmpty],[GLPiece B,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLBlock D,GLEmpty,GLEmpty,GLBlock D,GLEmpty,GLEmpty,GLEmpty,GLEmpty,GLPiece C],[GLEmpty,GLPiece D,GLEmpty,GLPiece D,GLEmpty,GLPiece D,GLGoal DirDown A,GLGoal DirDown A,GLPiece D,GLEmpty,GLPiece D,GLEmpty,GLPiece D,GLEmpty]]