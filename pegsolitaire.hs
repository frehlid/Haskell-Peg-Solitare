import Data.Array
import Data.Maybe
import Data.List
import Data.Maybe (isJust)
import Control.Monad.RWS (MonadState(state))
type Board = [[BoardEntry]]
type BoardEntry = (Coordinate, Int) -- status is one of 1=filled 0=empty -1=invalid
type Coordinate = String
type Move = (Coordinate, Coordinate, Coordinate) -- (Start, Middle, End)

-- Based off of magicsum.hs from class
data State = State Board [Move]
    deriving (Ord, Eq, Show)

data Result = EndOfGame State
            | ContinueGame State
    deriving (Eq, Show)

type Game = Move -> State -> Result


-- Game logic
--peg_solitaire :: Game

-- Generate starting board and starting list of moves
--peg_solitaire_start = State starting_board

-- Returns a board with all pegs filled except the center
starting_board :: Board
starting_board =
  [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("A3",1),("A4",1),("A5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("B3",1),("B4",1),("B5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("C1",1),("C2",1),("C3",1),("C4",1),("C5",1),("C6",1),("C7",1),("",-1),("",-1)],
  [("",-1),("",-1),("D1",1),("D2",1),("D3",1),("D4",0),("D5",1),("D6",1),("D7",1),("",-1),("",-1)],
  [("",-1),("",-1),("E1",1),("E2",1),("E3",1),("E4",1),("E5",1),("E6",1),("E7",1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("F3",1),("F4",1),("F5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("G3",1),("G4",1),("G5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]

--Nate generates legal moves
legal_moves :: Int -> Int -> Board -> [Maybe Move]
legal_moves row col board
  | snd (board!!row!!col) == -1 = []
  | otherwise = [legal_moves_helper row col board x | x <- ["up", "right", "down", "left"]]

legal_moves_helper :: Int -> Int -> Board -> String -> Maybe Move
legal_moves_helper row col board dir
  | dir=="up" = if snd (board!!(row-2)!!col) /= -1
                  then Just (fst(board!!row!!col), fst(board!!(row-1)!!col), fst(board!!(row-2)
                  !!col))
                  else Nothing
  | dir=="right" = if snd (board!!row!!(col+2)) /= -1
                    then Just (fst(board!!row!!col), fst(board!!row!!(col+1)), fst(board!!row!!
                    (col+2)))
                    else Nothing
  | dir=="down" = if snd (board!!(row+2)!!col) /= -1
                    then Just (fst(board!!row!!col), fst(board!!(row+1)!!col), fst(board!!(row+2)
                    !!col))
                    else Nothing
  | dir=="left" = if snd(board!!row!!(col-2)) /= -1
                    then Just (fst(board!!row!!col), fst(board!!row!!(col-1)), fst(board!!row!!
                    (col-2)))
                    else Nothing

generate_legal_moves :: Board -> [Move]
generate_legal_moves starting_board =
  do
    let arr = [legal_moves row col starting_board | row <- [2..8], col <- [2..8]]
    let processed = concat [[fromJust x | x <- row, isJust x] | row <- arr, row/=[]]
    return processed!!0


-- dieter
update_valid_moves :: Board -> [Move]
-- For each board entry, try all 4 legal moves?
    -- Starting position == 1
    -- Middle position == 1
    -- Ending position == 0
update_valid_moves board = 
  let legal = generate_legal_moves starting_board
   in filter (\(start, mid, end) -> 
        let startEntry = get_board_entry board start
            midEntry = get_board_entry board mid
            endEntry = get_board_entry board end
        in isJust startEntry && isJust midEntry && isJust endEntry -- check that the coord exists
           && snd (fromJust startEntry) == 1 -- a peg must be in the starting hole
           && snd (fromJust midEntry) == 1 -- there must be a peg to jump over
           && snd (fromJust endEntry) == 0) -- there must be a spot for the peg to land
           legal 

-- Takes a game board and a coordinate, returns BoardEntry for that coordinate 
get_board_entry :: Board -> Coordinate -> Maybe BoardEntry 
get_board_entry board coord =
    let row = (fromEnum (head coord))-65
    in find (match_entry coord) (board!!(row+2))


--Nate
--display_board :: Board -> IO ()
--display_board board =


--get_move :: IO () -> Move
--get_move =

check_valid_move :: State -> Move -> Bool
check_valid_move st mv = True

-- Returns a new game state with the move performed
update_game_state :: State -> Move -> State
update_game_state (State board moves) move =
  let newBoard = perform_move board move
      newMoves = update_valid_moves newBoard 
  in  State newBoard newMoves 

-- Performs the given move on the game state
-- Removes the peg from the Starting and Middle positions
-- Adds a peg to the end position
perform_move :: Board -> Move -> Board 
perform_move board (start, middle, end) =
  let b1 = modify_board_position board start 0
      b2 = modify_board_position b1 middle 0
  in modify_board_position b2 end 1


-- Consumes a board, coordinate, and state
-- Returns a new board with the status set to newState
modify_board_position :: Board -> Coordinate -> Int -> Board
modify_board_position board coord newState =
  let row = get_row_number coord 
      index = case findIndex (match_entry coord) (board!!(row+2)) of
              Just index -> index
              Nothing -> 0
      (start,elem:rest) = splitAt index (board!!(row+2)) -- Split list
      newRow = start ++ ((fst elem), newState) : rest -- Modify the row and splice it together
  in take (row + 2) board ++ [newRow] ++ drop (row + 3) board -- Replace the old row with the new row


-- Consumes a coordinate and returns the corresponding row number
-- Does not accounting for padding in the game board
get_row_number :: Coordinate -> Int
get_row_number coord = (fromEnum (head coord)) - fromEnum('A')

-- Return true if the coordiante matches the coordiante in the board entry
match_entry :: Coordinate -> BoardEntry -> Bool
match_entry coord entry = fst entry == coord

win :: State -> Bool
win st = True
