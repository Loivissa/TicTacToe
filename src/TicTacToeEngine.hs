module TicTacToeEngine (Board (..), Field (..), GameStatus (..), newBoard, makeAMove, seeGameStatus) where
import Data.Ix
import Data.Array

--a!i - element with an index i from array a
--a // [(i1,e1), (i2,e2)] - returns an copy of a except that a!i1 is replacd by e1 and a!i2 is replaced by e2
--listArray (i1,i2) [e1,e2,e3...] - returns an array with indexes from i1 to i2 and elements e1,e2,e3 etc.
--elems arr - returns a list of elements contained in arr
newBoard :: Board --returns a new, empty board
-- | @makeAMove@ returns @Just Board@ after making a specified move or @Nothing@ if move is impossible. Move is specified by the current game represented by a @Board@, a @Field@ indicating whether we want to place a @Circle@ or a @Cross@ and @(Int,Int)@ index (i,j) with i representing a row number and j representing a column number. Example: @makeAMove board Circle (0,2)@ "puts" a @Circle@ in field in row 0, column 2, if possible, returns @Nothing@ if impossible. (See @Board@ for the layout of the board)
makeAMove :: Board -> Field -> (Int,Int) -> Maybe Board
-- | takes a @Board@, returns information about status (@GameStatus@): is it won by circles, by crosses, is it a stalemate or is it ongoing? (See @GameStatus@)
seeGameStatus :: Board -> GameStatus 

-- | @Field@ describes a possible state of a field on the playing board (represented by @Board@). A @Field@ can be @Empty@, can have a @Cross@ or a @Circle@.
data Field = Empty | Cross | Circle deriving(Show)
instance Eq Field where
 Empty == Empty = True
 Cross == Cross = True
 Circle == Circle = True
 _ == _ = False

 -- | @GameStatus@ is a variable describing the state of the game - did it end or not and what is a result? @InProgress@ indicates that the game hasn't ended yet. @CircleWon@, @CrossWon@ an @Stalemate@ indicate both the fact that the game has ended and the result.
data GameStatus = InProgress | CrossWon | CircleWon | Stalemate deriving(Show)
instance Eq GameStatus where
 InProgress == InProgress = True
 CrossWon == CrossWon = True
 CircleWon == CircleWon = True
 Stalemate == Stalemate = True
 _ == _ = False

 -- | @Board@ represents the board for the Tic Tac Toe game. Starting board is represented by @newBoard@, which is an empty 3x3 Tic Tac Toe board. It's rows and columns are numbered from 0 to 2. Those numbers are used to specify where do you want to move. (See @makeAMove@ for the exact way to specify where do you want to place your mark)
type Board = Array (Int,Int) Field

newBoard = listArray ((0,0),(2,2)) [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

makeAMove _ Empty _ = Nothing
makeAMove board f (i,j) = if(i>=0 && j>=0 && i<=2 && j<=2 && board!(i,j) == Empty) then Just (board // [((i,j),f)]) else Nothing

seeGameStatus board = if (filledRow Cross board || filledColumn Cross board || filedDiagonal Cross board) then CrossWon else (if (filledRow Circle board || filledColumn Circle board || filedDiagonal Circle board) then CircleWon else ( if (allFilled board) then Stalemate else InProgress ) )
 where
 checkIfEquals :: Field -> Board -> [(Int,Int)] -> Bool
 checkIfEquals _ _ [] = True
 checkIfEquals f board ((i,j):tail) = (f == (board!(i,j))) && checkIfEquals f board tail 
 checkIfAnyEqual :: Field -> Board -> [[(Int,Int)]] -> Bool
 checkIfAnyEqual _ _ [] = False
 checkIfAnyEqual f board (((i,j):t):tail) = if (checkIfEquals f board ((i,j):t)) then True else checkIfAnyEqual f board tail
 filledRow :: Field -> Board -> Bool
 filledRow f board = checkIfAnyEqual f board [[(i,j) | j <- [0..2]] | i<-[0..2]] 
 filledColumn :: Field -> Board -> Bool
 filledColumn f board = checkIfAnyEqual f board [[(i,j) | i <- [0..2]] | j<-[0..2]] 
 filedDiagonal :: Field -> Board -> Bool
 filedDiagonal f board = checkIfAnyEqual f board [[(0,0),(1,1),(2,2)],[(0,2),(1,1),(2,0)]]
 allFilled :: Board -> Bool
 allFilled board = allFilledOnList (elems board)
  where
  allFilledOnList :: [Field] -> Bool
  allFilledOnList [] = True
  allFilledOnList (a:at) = if(a==Empty) then False else (allFilledOnList at)