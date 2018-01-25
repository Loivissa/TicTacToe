module Main where
import Test.HUnit
import TicTacToeEngine
import TicTacToePlayer
import TicTacToeBoard

main :: IO ()
main = do
  defaultMain (testGroup "TictacToe Tests" [Test])

sayYoTest :: TestTree
sayYoTest = testCase "Testing playGame"
  (assertEqual "Cross should be X!" "X" (convertFieldToString Cross))
