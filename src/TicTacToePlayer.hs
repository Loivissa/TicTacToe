module TicTacToePlayer(sensiblyDecideHowToMove, lazilyDecideHowToMove, oneDimIndexToTwoDim) where
import TicTacToeEngine
import Data.Maybe
import Data.Array

isGameFinished :: Board -> Bool
isGameFinished board = ((seeGameStatus board) /= InProgress)

oneDimIndexToTwoDim :: Int -> (Int,Int)
oneDimIndexToTwoDim index = ( (index `div` 3), (index `mod` 3) )

otherPlayersField :: Field -> Field
otherPlayersField Cross = Circle
otherPlayersField Circle = Cross
otherPlayersField _ = Empty

maybeChecker :: Maybe a -> Bool
maybeChecker Nothing = False
maybeChecker _ = True

checkOnList :: Field -> [Field] -> Maybe Int
checkOnList Cross (Empty:Cross:Cross:[]) = Just 0
checkOnList Cross (Cross:Empty:Cross:[]) = Just 1
checkOnList Cross (Cross:Cross:Empty:[]) = Just 2
checkOnList Circle (Empty:Circle:Circle:[]) = Just 0
checkOnList Circle (Circle:Empty:Circle:[]) = Just 1
checkOnList Circle (Circle:Circle:Empty:[]) = Just 2
checkOnList _ _ = Nothing


firstEmptyPlace :: Board -> (Int,Int)
firstEmptyPlace board = oneDimIndexToTwoDim (oneDimIndex board) 
 where
 loop :: [Field] -> Int -> Int
 loop [] acc = acc
 loop (a:at) acc = if (a==Empty) then acc else loop at (acc+1)
 oneDimIndex board = loop (elems board) 0

  -- | @lazilyDecideHowToMove@ takes a @Board@ and a @Field@ representing whether it is a @Cross@ or a @Circle@ move. It returns a new @Board@ after making a first possible move - either as a @Just Board@ or @Nothing@ if there is no possible move.
lazilyDecideHowToMove :: Field -> Board -> Maybe Board
lazilyDecideHowToMove f board = makeAMove board f (firstEmptyPlace board)

 -- | @sensiblyDecideHowToMove@ takes a @Board@ and a @Field@ representing whether it is a @Cross@ or a @Circle@ move. It returns a new @Board@ (either as a @Just Board@ or @Nothing@ if there is no possible move) after making a move that should either seize victory if it's immediately available, prevent the enemy from doing the same or - if none of those are available - simply places a required @Cross@ or @Circle@ in the first possible place.
sensiblyDecideHowToMove :: Field -> Board -> Maybe Board
sensiblyDecideHowToMove f board =
 if((findColumnWithOneWindow f board) /= Nothing) --check if you can win by getting 3 in the row in a column...
  then (makeAMove board f (fromJust(findColumnWithOneWindow f board)) )
  else (if((findRowWithOneWindow f board) /= Nothing) -- ...or in a row
           then (makeAMove board f (fromJust(findRowWithOneWindow f board)) )
           else (if(findDiagonalWithOneWindow f board /= Nothing) --or on a diagonal
                    then (makeAMove board f (fromJust(findDiagonalWithOneWindow f board)))
                    else (if((findColumnWithOneWindow (otherPlayersField f) board) /= Nothing) --check if you can block your opponent's 3 in the row
                             then (makeAMove board f (fromJust(findColumnWithOneWindow (otherPlayersField f) board)))
                             else (if((findRowWithOneWindow (otherPlayersField f) board) /= Nothing)
                                      then (makeAMove board f (fromJust(findRowWithOneWindow (otherPlayersField f) board)))
                                       else (if(findDiagonalWithOneWindow (otherPlayersField f) board /= Nothing)
                                               then(makeAMove board f (fromJust(findDiagonalWithOneWindow (otherPlayersField f) board)))
                                                   else(makeAMove board f (firstEmptyPlace board))))))) --no chance to win for you or your opponent this round, place your mark wherever
 where
 loopingChecker :: (Field -> Board -> Int -> Maybe (Int,Int)) -> Field -> Board -> Int -> Maybe(Int,Int)
 loopingChecker _ _ _ 3 = Nothing
 loopingChecker (fun) field board acc = if((fun field board acc) /= Nothing) then (fun field board acc) else (loopingChecker (fun) field board (acc+1))
 findDiagonalWithOneWindow :: Field -> Board -> Maybe (Int,Int)
 findDiagonalWithOneWindow f b = 
    if(maybeChecker(checkOnList f ((b!(0,0)):(b!(1,1)):(b!(2,2)):[]))) 
      then (Just (fromJust(checkOnList f ((b!(0,0)):(b!(1,1)):(b!(2,2)):[])), fromJust(checkOnList f ((b!(0,0)):(b!(1,1)):(b!(2,2)):[]))))
      else (if maybeChecker(checkOnList f ((b!(0,2)):(b!(1,1)):(b!(2,0)):[]))
                then (Just (fromJust(checkOnList f ((b!(0,2)):(b!(1,1)):(b!(2,0)):[])),translateDiagonalIndex(fromJust(checkOnList f ((b!(0,2)):(b!(1,1)):(b!(2,0)):[])))))
                else Nothing)
   where
   translateDiagonalIndex :: Int -> Int
   translateDiagonalIndex 0 = 2
   translateDiagonalIndex 1 = 1
   translateDiagonalIndex 2 = 0
   translateDiagonalIndex _ = 0
 findColumnWithOneWindow :: Field -> Board -> Maybe (Int,Int)
 findColumnWithOneWindow f board = loopingChecker (checkColumn) f board 0
  where
  checkColumn :: Field -> Board -> Int -> Maybe (Int,Int)
  checkColumn f board cIndex = 
    if(maybeChecker (checkColumnOnList f board cIndex))
      then (Just (oneDimIndexToTwoDim(fromJust(checkColumnOnList f board cIndex) + 3*cIndex)))
       else Nothing
   where
   checkColumnOnList :: Field -> Board -> Int -> Maybe Int
   checkColumnOnList f b cIndex = checkOnList f ((b!(0,cIndex)):(b!(1,cIndex)):(b!(2,cIndex)):[])
 findRowWithOneWindow :: Field -> Board -> Maybe (Int,Int)
 findRowWithOneWindow f board = loopingChecker (checkRow) f board 0
  where
  checkRow :: Field -> Board -> Int -> Maybe (Int,Int)
  checkRow f board rIndex = 
    if(maybeChecker (checkRowOnList f board rIndex)) 
      then (Just (oneDimIndexToTwoDim(fromJust(checkRowOnList f board rIndex) + 3*rIndex)))
      else Nothing
   where
   checkRowOnList :: Field -> Board -> Int -> Maybe Int
   checkRowOnList f b rIndex = checkOnList f ((b!(rIndex,0)):(b!(rIndex,1)):(b!(rIndex,2)):[])