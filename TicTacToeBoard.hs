module TicTacToeBoard (convertFieldToString, boardToString, next, eliminate, help) where
import TicTacToeEngine
import Data.Array

convertFieldToString :: Field -> String
convertFieldToString Cross = "X"
convertFieldToString Circle = "O"
convertFieldToString Empty = " "

boardToString :: Board -> String
boardToString b = ",,,,,,,\n" ++ "|" ++ (convertFieldToString (b!(0,0))) ++ "|" ++  (convertFieldToString (b!(0,1))) ++ "|" ++ (convertFieldToString (b!(0,2))) ++ "|\n" ++ "|" ++ (convertFieldToString (b!(1,0))) ++ "|" ++  (convertFieldToString (b!(1,1))) ++ "|" ++ (convertFieldToString (b!(1,2))) ++ "|\n" ++ "|" ++ (convertFieldToString (b!(2,0))) ++ "|" ++  (convertFieldToString (b!(2,1))) ++ "|" ++ (convertFieldToString (b!(2,2))) ++ "|\n" ++  "'''''''"

help :: String
help = ",,,,,,,\n|0|1|2|\n|3|4|5|\n|6|7|8|\n'''''''"


next :: Field ->  Field
next Cross = Circle
next Circle = Cross
next Empty = Empty

eliminate :: Maybe Board -> Board
eliminate Nothing = newBoard
eliminate (Just a) = a
