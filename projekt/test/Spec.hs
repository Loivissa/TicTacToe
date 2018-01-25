module Main (main) where
import Test.HUnit
import TicTacToeEngine
import TicTacToePlayer
import TicTacToeBoard
import System.Exit
import Data.Maybe
import Data.Array

main :: IO ()
main = do
    counts <- runTestTT (test [
            convertingCrossTest,
			achievingVictoryTest,
			detectFullBoardTest,
			seeCirclesVictoryTest
            ])
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