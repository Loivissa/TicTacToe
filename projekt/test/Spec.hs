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
  (assertEqual "Should say Yo to Friend!" "X" (convertFieldToString Cross))
