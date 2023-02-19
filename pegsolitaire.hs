-- Game
type Board = [[BoardEntry]]
type BoardEntry = (Coordinate, status) -- status is one of 1=filled 0=empty -1=invalid
type Coordinate = [Char]
type Move = (Coordinate, Coordinate) -- (Start, End)

-- Based off of magicsum.hs from class
data State = State Board [Move]
    deriving (Ord, Eq, Show)

data Result = EndOfGame State
            | ContinueGame State
    deriving (Eq, Show)

type Game = Move -> State -> Result


-- Game logic
peg_solitaire :: Game

-- Generate starting board and starting list of moves
peg_solitaire_start = State generate_starting_board

-- Returns a board with all pegs filled except the center
generate_starting_board :: Board
generate_starting_board = [
[("A1", -1), ("A2", -1), ("A3", 1), ("A4", 1), ("A5", 1), ("A6", -1), ("A7", -1)],
[("B1", -1), ("B2", -1), ("B3", 1), ("B4", 1), ("B5", 1), ("B6", 1), ("B7", 1)],
[("C1", 1), ("C2", 1), ("C3", 1), ("C4", 1), ("C5", 1), ("C6", 1), ("C7", 1)],
[("D1", 1), ("D2", 1), ("D3", 1), ("D4", 0), ("D5", 1), ("D6", 1), ("D7", 1)],
[("E1", 1), ("E2", 1), ("E3", 1), ("E4", 1), ("E5", 1), ("E6", 1), ("E7", 1)],
[("F1", -1), ("F2", -1), ("F3", 1), ("F4", 1), ("F5", 1), ("F6", 1), ("F7", 1)],
[("G1", -1), ("G2", -1), ("G3", 1), ("G4", 1), ("G5", 1), ("G6", -1), ("G7", -1)],
[("H1", -1), ("H2", -1), ("H3", 1), ("H4", 1), ("H5", 1), ("H6", -1), ("H7", -1)]
]
--Nate generates legal moves

-- dieter
generate_valid_moves :: Board -> [Move]
-- For each board entry, try all 4 legal moves?
    -- Starting position == 1
    -- Middle position == 1
    -- Ending position == 0
generate_valid_moves Board =
    -- Filter list of valid moves, for each move, check the above conditions

--Nate
display_board :: State -> IO ()

get_move :: IO () -> Move

update_valid_moves :: State -> Move -> State

check_valid_move :: State -> Move -> Bool

update_game_state :: State -> Move -> State

win :: State