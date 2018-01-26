module Main (main) where
import Test.HUnit
import TicTacToeEngine
import TicTacToePlayer
import TicTacToeBoard
import System.Exit
import Data.Maybe
import Data.Array
import Test.QuickCheck

main :: IO ()
main = do
    counts <- runTestTT (test [
            convertingCrossTest,
            achievingVictoryTest,
            detectFullBoardTest,
            seeCirclesVictoryTest,
            tests
            ])
    quickCheck inProgressTest
    quickCheck cannotPutIntoFullBoardTest
    quickCheck firstEmptyPlaceTest
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure


convertingCrossTest :: Test
convertingCrossTest = TestCase  $ assertEqual "Cross should be X!" "X" (convertFieldToString Cross)

achievingVictoryTest :: Test
achievingVictoryTest = TestCase  $ assertEqual "That should have been won!" CrossWon (seeGameStatus (fromJust(sensiblyDecideHowToMove(Cross)(listArray ((0,0),(2,2)) [Empty,Empty,Empty,Empty,Empty,Empty,Cross,Empty,Cross]))))

detectFullBoardTest :: Test
detectFullBoardTest = TestCase $ assertEqual "This shouldn't work!" Nothing (sensiblyDecideHowToMove(Cross)(listArray ((0,0),(2,2)) [Cross,Circle,Cross,Circle,Cross,Cross,Cross,Circle,Cross]))

seeCirclesVictoryTest :: Test
seeCirclesVictoryTest = TestCase $ assertEqual "But the circles won!" CircleWon (seeGameStatus(listArray ((0,0),(2,2)) [Cross, Empty, Circle, Cross, Circle, Empty, Circle, Empty, Empty]))

tests = TestList [ "test boasrdToString"   ~: ",,,,,,,\n| | | |\n| | | |\n| | | |\n'''''''" ~=? (boardToString newBoard)
                 , "test firstEmptyPlace"  ~: (0,1) ~=? firstEmptyPlace (listArray ((0,0),(2,2)) [Cross, Empty, Circle, Cross, Circle, Empty, Circle, Empty, Empty])
                 , "test otherPlayersField" ~: Cross ~=? otherPlayersField Circle
                 , "test oneDimIndexToTwoDim" ~: (1,1) ~=? oneDimIndexToTwoDim 4
                 ]

fieldToBool :: Field -> Bool
fieldToBool Circle = False
fieldToBool _ = True

boolToField:: Bool -> Field
boolToField True = Cross
boolToField False = Circle
 
inProgressTest :: Bool -> Bool -> Bool
inProgressTest b1 b2 = (seeGameStatus(eliminate(lazilyDecideHowToMove(boolToField(b1))(newBoard))) == seeGameStatus(eliminate(lazilyDecideHowToMove(boolToField(b2))(newBoard))))

intToField :: Int -> Field
intToField i = if ((i `mod` 2)==0) then Cross
                                   else Circle

intsToBoard :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Board
intsToBoard a b c d e f g h i = listArray ((0,0),(2,2)) [intToField a, intToField b, intToField c, intToField d, intToField e, intToField f, intToField g, intToField h, intToField i]

cannotPutIntoFullBoardTest :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
cannotPutIntoFullBoardTest a b c d e f g h i j k l m n o p q r = (eliminate(makeAMove (intsToBoard a b c d e f g h i) Cross (0,0)) == eliminate(makeAMove (intsToBoard j k l m n o p q r) Cross (0,0)))


firstEmptyPlaceTest:: Int -> Int -> Bool
firstEmptyPlaceTest a b = (firstEmptyPlace (eliminate(makeAMove newBoard (intToField b) (0,0))) == firstEmptyPlace (eliminate(makeAMove newBoard (intToField b) (0,0))))
