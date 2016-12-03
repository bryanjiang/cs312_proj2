-- CPSC 312 - 2016 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play
import Prison
import System.IO
--import MiniMax

type TournammentState = (Int) -- your points

play :: Game -> Result -> Player -> TournammentState -> IO TournammentState

play game start opponent tournament_state =
  let (points) = tournament_state in
	   do
      putStrLn ("Tournament results: "++ show points++ " points ")
      putStrLn "Rules: choose 1 for cooperative and 0 for betray. when one player betrays and the other cooperated. The traitor would get 5 points, if both cooperative, both would get 3 points. And last both players will get 1 if they betray each other"
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if (read line :: Int)==0
      then
            person_play game start opponent tournament_state
      else if (read line :: Int)==1
           then
               computer_play game start opponent tournament_state
            else
               return tournament_state

person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play

person_play game (EndOfGame y) opponent (points) =
   do
      putStrLn "Computer won!"
      play game (game Start) opponent (points + y)

person_play game (ContinueGame state avail) opponent tournament_state =
   do
      putStrLn ("State is "++show state++" choose one of "++show avail)
      line <- getLine
      computer_play game (game (Move (read line :: AMove) state)) opponent tournament_state



computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent tournament_state
-- person has played, the computer must now play

computer_play game (EndOfGame x) opponent (points) =
   do
      putStrLn "You won!"
      play game (game Start) opponent (points + x)

computer_play game result opponent tournament_state =
      let ContinueGame state _ = result
          opponent_move = opponent game result
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game (Move opponent_move state)) opponent tournament_state
      

-- play prison (prison Start) simple_player (0)
-- play prison (prison Start) mm_player (0) -- todo: filled this with players with different strategies

