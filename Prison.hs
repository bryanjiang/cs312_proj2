-- CPSC 312 - 2016 - Games in Haskell
module Prison where

-- To run it, try:
-- ghci
-- :load Prison

type AMove = Int                  -- a move for a player
type State = ([AMove], [AMove])   -- (mine,other's)

data Action = Move AMove State   -- do AMove in State
            | Start              -- returns starting state

data Result = EndOfGame Int        -- end of game
            | ContinueGame State [AMove]   -- continue with new state, and list of possible moves
         deriving (Eq, Show)

type Game = Action -> Result

type Player = Game -> Result -> AMove

------ prison game -------

prison :: Game
prison (Move move (mine, others))
    | length mine + length others == 10 && (win mine others) < 0  = EndOfGame 2      -- i lose
    | length mine + length others == 10 && (win mine others) > 0  = EndOfGame 1     -- i win
    | length mine + length others == 10 && (win mine others) == 0  = EndOfGame 0      -- draw
    | otherwise                         =
          ContinueGame (others, move:mine)
                 [act | act <- [0..1]]

prison Start = ContinueGame ([],[]) [0..1]

-- win n ns = if you cooperate while the other betray, you lose 5 points. gain 5 points vice versa
-- no changes if both players choose the same path
win [] [] = 0
win [] _ = 0
win _ [] = 0
win (x1:x1s) (x2:x2s)
	| x1==1 && x2==0 = -5 + win x1s x2s
	| x1==0 && x2==1 = 5 + win x1s x2s
	|otherwise = win x1s x2s

------- A Player -------

simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
simple_player _ (ContinueGame _ avail) = head [e | e <- [0..1]]
simple_player2 _ (ContinueGame _ avail) = head [e | e <- [1..1]]

-- Test cases
-- magicsum Start
-- magicsum (Move 6 ([5,3],[2,7]))
-- magicsum (Move 3 ([5,7],[2,9]))