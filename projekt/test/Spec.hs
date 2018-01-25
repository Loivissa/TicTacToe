module Main (main) where
import Test.HUnit
import TicTacToeEngine
import TicTacToePlayer
import TicTacToeBoard
import System.Exit

main :: IO ()
main = do
    counts <- runTestTT (test [
            convertingCrossTest
            ])
    if (errors counts + failures counts == 0)
        then exitSuccess
        else exitFailure


convertingCrossTest :: Test
convertingCrossTest = TestCase  $ assertEqual "Cross should be X!" "X" (convertFieldToString Cross)
