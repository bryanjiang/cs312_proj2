-- CPSC 312 - 2016 - Games in Haskell
module Minimax where

-- To run it, try:
-- ghci
-- :load Minimax

import Prison


----   Determining the best move  ---
minimax:: Game -> Result -> (Int, AMove)
-- minimax game result   =>  (value_to_player, move)
minimax game (ContinueGame st avail)  =
      maximum [value game (game (Move amove st)) amove
               | amove <- avail]

-- value game result move = (value,move) for current player of the state after move given result
value:: Game -> Result -> AMove -> (Int, AMove)
value _  (EndOfGame val) move = (val,move)
value game res move = 
   let (val,_) = minimax game res   -- (value,move) for next player
   in (-val,move)  -- value for current player is negative of value of the other player

-- to find the best opening move
-- minimax magicsum (magicsum Start)

--Try
-- minimax magicsum (ContinueGame ([1,5],[3,8]) [2,4,6,7,9])
-- minimax magicsum (ContinueGame ([1],[8,5]) [2,3,4,6,7,9])
-- minimax magicsum (ContinueGame ([8,5],[1,2]) [3,4,6,7,9])
-- minimax magicsum (ContinueGame ([5],[3,8]) [1,2,4,6,7,9])

mm_player:: Player
mm_player game result = snd ( minimax game result)