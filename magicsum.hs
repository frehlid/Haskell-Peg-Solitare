-- CPSC 312 - 2023 - Games in Haskell
----- Same as Magic_sum except that the state has ordered lists

module MagicSum_ord where

-- To run it, try:
-- ghci
-- :load MagicSum_ord

data State = State InternalState [Action]  -- internal_state available_actions
         deriving (Ord, Eq, Show)

data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action

------ The Magic Sum Game -------

data Action = Action Int                   -- a move for a player is just an Int
         deriving (Ord,Eq)
type InternalState = ([Action],[Action])   -- (self,other)

-- insert into a sorted list
insert :: Ord a => a -> [a] -> [a]
insert e [] = [e]
insert e (h:t)
  | e <= h = (e:h:t)
  | otherwise = h: (insert e t)


magicsum :: Game
magicsum move (State (mine,others) available) 
    | win move mine                = EndOfGame 1  magicsum_start   -- agent wins
    | available == [move]          = EndOfGame 0  magicsum_start     -- no more moves, tie
    | otherwise                    =
          ContinueGame (State (others,(insert move mine))   --  only difference with MagicSum.hs
                        [act | act <- available, act /= move])

-- win n ns = the agent wins if it selects n given it has already selected ns
win :: Action -> [Action] -> Bool
win (Action n) ns  = or [n+x+y==15 | Action x <- ns, Action y <- ns, x/=y]


magicsum_start = State ([],[]) [Action n | n <- [1..9]]

-- show and read actions just as the integer
instance Show Action where
    show (Action i) = show i
instance Read Action where
    readsPrec i st =  [(Action a,rst) | (a,rst) <- readsPrec i st]

------- A Player -------

simple_player :: Player
-- this player has an ordering of the moves, and chooses the first one available
simple_player (State _ avail) = head [Action e | e <- [5,6,4,2,8,1,3,7,9],
                                               Action e `elem` avail]


-- Test cases
-- magicsum (simple_player magicsum_start)  magicsum_start
-- a i = Action i  -- make it easier to type
-- as lst = [Action i | i <- lst]
-- magicsum (a 6) (State (as [3,5], as [2,7]) (as [1,4,6,8,9])) 
-- magicsum (a 3) (State (as [5,7], as [2,9]) (as [1,3,4,6,8])) 



-- Why is it called the "magic sum game"?
-- The following is a magic square:
-- 294
-- 753
-- 618
