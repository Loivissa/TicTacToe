module TicTacToeGame where
import TicTacToeEngine
import TicTacToePlayer
import Data.Array
import TicTacToeBoard
import Text.Read


-- | @playAGame@ starts a game based on players decisions (who starts, hard or easy mode)
playAGame :: IO()
playAGame = do
 putStrLn "Welcome!"
 putStrLn "easy or hard mode?"
 mode <- getLine
 putStrLn "Do you want to start? Y/N"
 answer<-getLine
 let board = newBoard
 if(answer=="Y")
  then  if(mode=="easy")
        then gameLoop 1 Cross 0 board -- Player move easy
            else if(mode=="hard")
             then gameLoop 1 Cross 1 board -- Player move hard
             else do
                    putStrLn "Mode must be either 'hard' or 'easy'."
                    playAGame
  else  if(mode=="easy" && answer == "N")
        then gameLoop 0 Cross 0 board -- Computer move easy
             else if(mode=="hard" && answer == "N")
                  then gameLoop 0 Cross 1 board -- Computer move hard
                  else if(mode=="easy" || mode == "hard")
                         then do 
                         putStrLn "Must be 'Y' or 'N'."
                         playAGame
                         else do
                         putStrLn "Mode must be either 'hard' or 'easy'."
                         playAGame



-- | @gameLoop@ handles game between player and computer. First instance interprets players intentions. Second makes moves as computer.
gameLoop :: Int -> Field -> Int -> Board -> IO()
gameLoop 1 f1 mod1 board11 = do
 putStrLn "Where?"
 putStrLn ("You are playing as " ++ (convertFieldToString f1))
 putStrLn help
 coordinates <-getLine
 let place = readMaybe coordinates :: Maybe Int 
 if (place == Nothing)
    then do 
         putStrLn (boardToString board11)
         putStrLn "Must be a number."
         gameLoop 1 f1 mod1 board11
    else do
    let board2 = (makeAMove board11 f1 (oneDimIndexToTwoDim (eliminate2 place)))
    let board3 = eliminate (board2)
    if (board2 == Nothing)
       then do
             putStrLn (boardToString board11)
             putStrLn "Wrong move"
             gameLoop 1 f1 mod1 board11
       else do
             putStrLn (boardToString board3)
             if(seeGameStatus board3 == CircleWon && f1==Circle)
             then putStrLn "You have won!"
             else if(seeGameStatus board3 == CrossWon && f1==Cross)
                   then putStrLn "You have won!"
                   else if(seeGameStatus board3 == CrossWon && f1==Circle)
                         then putStrLn "Computer has won!"
                         else if(seeGameStatus board3 == CrossWon && f1==Cross)
                               then putStrLn "Computer has won!"
                               else if(seeGameStatus board3 == Stalemate)
                                     then putStrLn "Stalemate!"
                                     else do
                                     gameLoop 0 (otherPlayersField f1) mod1 board3

gameLoop 0 f2 mod2 board12 = do
 let board2 = if(mod2==0) then (lazilyDecideHowToMove f2 board12) else (sensiblyDecideHowToMove f2 board12)
 let board3 = eliminate (board2)
 if (board2 == Nothing)
  then putStrLn "Klopsik."
  else do
       putStrLn (boardToString board3)
       if(seeGameStatus board3 == CircleWon && f2==Circle)
       then (putStrLn "Computer has won!")
       else if(seeGameStatus board3 == CrossWon && f2==Cross)
            then (putStrLn "Computer has won!")
            else if(seeGameStatus board3 == CrossWon && f2==Circle)
                 then (putStrLn "You have won!")
                 else if(seeGameStatus board3 == CrossWon && f2==Cross)
                      then (putStrLn "You have won!")
                      else if(seeGameStatus board3 == Stalemate)
                           then (putStrLn "Stalemate!")
                           else if (board2==Nothing)
                                then putStr "Stalemate!"
                                else do
                                     gameLoop 1 (otherPlayersField f2) mod2 board3



