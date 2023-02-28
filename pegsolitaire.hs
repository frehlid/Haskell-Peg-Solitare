import Data.Array
import Data.Maybe
import Data.List
import Data.Char (ord)
import Data.Maybe (isJust)
import HashTreeDict
import qualified Data.PQueue.Min as PQ
import Debug.Trace
import Control.Monad (foldM_)
import qualified Data.Set as Set

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


type Mem = Dict Int (Bool, [Move])

type Pq = PQ.MinQueue (Int, (State, [Move]))

-- USAGE --
-- To play from the start, use > playGame
-- To play from a state that is partially solved, use > play partialGameState

--------- SAMPLE BOARDS ---------
startingBoard :: Board -- Board used to start the game
startingBoard =
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


winBoard :: Board -- Winning board
winBoard = 
  [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("A3",0),("A4",0),("A5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("B3",0),("B4",0),("B5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("C1",0),("C2",0),("C3",0),("C4",0),("C5",0),("C6",0),("C7",0),("",-1),("",-1)],
  [("",-1),("",-1),("D1",0),("D2",1),("D3",0),("D4",1),("D5",0),("D6",0),("D7",0),("",-1),("",-1)],
  [("",-1),("",-1),("E1",0),("E2",1),("E3",1),("E4",0),("E5",0),("E6",0),("E7",0),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("F3",0),("F4",0),("F5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("G3",0),("G4",0),("G5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]


almostWinBoard :: Board -- Test board, close to winning
almostWinBoard =
  [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("A3",0),("A4",0),("A5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("B3",0),("B4",0),("B5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("C1",1),("C2",1),("C3",1),("C4",0),("C5",1),("C6",1),("C7",0),("",-1),("",-1)],
  [("",-1),("",-1),("D1",1),("D2",1),("D3",1),("D4",0),("D5",1),("D6",1),("D7",0),("",-1),("",-1)],
  [("",-1),("",-1),("E1",1),("E2",1),("E3",1),("E4",0),("E5",1),("E6",1),("E7",0),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("F3",0),("F4",0),("F5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("G3",0),("G4",0),("G5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]

waterlooBoard :: Board -- Board from waterloo resource see https://ece.uwaterloo.ca/~dwharder/aads/Algorithms/Backtracking/Peg_solitaire/
waterlooBoard =
    [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
    [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
    [("",-1),("",-1),("",-1),("",-1),("A3",0),("A4",0),("A5",0),("",-1),("",-1),("",-1),("",-1)],
    [("",-1),("",-1),("",-1),("",-1),("B3",0),("B4",1),("B5",0),("",-1),("",-1),("",-1),("",-1)],
    [("",-1),("",-1),("C1",0),("C2",0),("C3",1),("C4",0),("C5",1),("C6",0),("C7",0),("",-1),("",-1)],
    [("",-1),("",-1),("D1",0),("D2",0),("D3",1),("D4",0),("D5",0),("D6",1),("D7",0),("",-1),("",-1)],
    [("",-1),("",-1),("E1",0),("E2",0),("E3",0),("E4",0),("E5",0),("E6",0),("E7",0),("",-1),("",-1)],
    [("",-1),("",-1),("",-1),("",-1),("F3",0),("F4",0),("F5",0),("",-1),("",-1),("",-1),("",-1)],
    [("",-1),("",-1),("",-1),("",-1),("G3",0),("G4",0),("G5",0),("",-1),("",-1),("",-1),("",-1)],
    [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
    [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]

almostLoseBoard :: Board -- Test board, close to losing
almostLoseBoard =
  [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("A3",0),("A4",0),("A5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("B3",0),("B4",0),("B5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("C1",0),("C2",0),("C3",1),("C4",0),("C5",0),("C6",0),("C7",1),("",-1),("",-1)],
  [("",-1),("",-1),("D1",0),("D2",1),("D3",1),("D4",0),("D5",0),("D6",0),("D7",0),("",-1),("",-1)],
  [("",-1),("",-1),("E1",0),("E2",0),("E3",0),("E4",0),("E5",0),("E6",0),("E7",0),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("F3",0),("F4",0),("F5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("G3",0),("G4",0),("G5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]

partialBoard :: Board -- Test board, relatively close to winning
partialBoard =
  [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("A3",0),("A4",0),("A5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("B3",0),("B4",0),("B5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("C1",0),("C2",1),("C3",0),("C4",1),("C5",1),("C6",1),("C7",1),("",-1),("",-1)],
  [("",-1),("",-1),("D1",0),("D2",0),("D3",0),("D4",0),("D5",1),("D6",1),("D7",1),("",-1),("",-1)],
  [("",-1),("",-1),("E1",1),("E2",1),("E3",0),("E4",0),("E5",1),("E6",1),("E7",1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("F3",0),("F4",0),("F5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("G3",0),("G4",0),("G5",1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]


--------- SAMPLE STATES ---------
startState = (State startingBoard (generateValidMoves startingBoard))
partialGameState = (State partialBoard (generateValidMoves partialBoard))
endGameStateWin = (State almostWinBoard (generateValidMoves almostWinBoard))
endGameStateLose = (State almostLoseBoard (generateValidMoves almostLoseBoard))


--------- PLAY ---------
playEndGameWin = play endGameStateWin
playEndGameLose = play endGameStateLose
playPartialGame = play partialGameState
playGame = play startState -- play from the beginning

play :: State -> IO ()
play (State bd moves) =
  do
    displayBoard bd
    move <- parseMove moves
    displayMove bd move
    putStrLn "      |  |  |       "
    putStrLn "      V  V  V       "
    putStrLn "                    "
    let newState = updateGameState (State bd moves) move
    displayBoard (getBoard newState)
    if (win newState) then do
        displayBoard (getBoard newState)
        putStrLn "You win!"
    else if (lose newState) then do
        displayBoard (getBoard newState)
        putStrLn "You lose :("
    else do
        continue newState

continue :: State -> IO ()
continue newState = do
               putStrLn "Please select one of the following options:"
               putStrLn "(C)ontinue - (S)top - (F)inish"
               choiceUnFixed <- getLine
               let choice = fixdel choiceUnFixed
               if (choice == "Continue" || choice == "C" || choice == "c") then
                  play newState
               else if (choice == "Stop" || choice == "quit" || choice == "S" || choice == "s") then
                   putStrLn "Bye!"
               else if (choice == "Finish" || choice == "F" || choice == "f") then do
                   putStrLn "This may take a while, are you sure? (y/n)"
                   ans <- getLine
                   if (ans == "y" || ans == "Y" || ans == "Yes") then do
                       putStrLn "What implementation would you like to use? ((1) DFS(Tree) / (2) DFS(List) / (3) BestFS(Manhattan))"
                       choiceUnFixed <- getLine
                       let choice = fixdel choiceUnFixed
                       solveState newState choice
                   else do
                        continue newState
               else do
                    putStrLn "That is not a valid choice, please try again"
                    continue newState

solveState :: State -> String -> IO ()
solveState state choice =
    if (choice == "DFS(Tree)" || choice == "1") then
        let ((bool, moves), mem) = search state emptyDict 31 in do
        if (bool == True) then do
            displayAllMoves state moves
        else do
            putStrLn "This board is not solvable"
            displayBoard (getBoard state)
    else if (choice == "DFS(List)" || choice == "2") then
        let (bool, moves, mem) = nateSolve state [] [] in do
        if (bool == True) then do
            displayAllMoves state moves
        else do
            putStrLn "This board is not solvable"
            displayBoard (getBoard state)
    else if (choice == "BestFS(Manhattan)" || choice == "3") then
        let (bool, moves, mem, queue) = solve_h state in do
        if (bool == True) then do
            displayAllMoves state moves
        else do
            putStrLn "This board is not solvable"
            displayBoard (getBoard state)
    else do
        putStrLn "That is not a valid choice, please try again"
        putStrLn "What implementation would you like to use? ((1) DFS(Tree) / (2) DFS(List) / (3) BestFS(Manhattan))"
        choiceUnFixed <- getLine
        let choice = fixdel choiceUnFixed
        solveState state choice

-- Prompts the user to enter the coordinates of a move
-- Returns the move if it is in the list of available moves, otherwise prompts the user to try again
parseMove :: [Move] -> IO Move
parseMove moves =
    do
        putStrLn "Please enter the starting coordinate of the move, or (H)int to see available moves:"
        startCoord <- getCoordinate moves
        putStrLn "Please enter the ending coordinate of the move, or (H)int to see a available moves:"
        endCoord <- getCoordinate moves
        let move = getMove startCoord endCoord moves
        if isJust move then do
            return (fromJust move)
        else do
            let line = (startCoord ++ " to " ++ endCoord ++ " is not a valid move. Please try again:")
            putStrLn line
            parseMove moves

-- Takes a starting coordinate, an ending coordinate, and a list of available moves
-- Returns the move if it is in the list of available moves, otherwise returns Nothing
getMove :: String -> String -> [Move] -> Maybe Move
getMove start end  moves =
    let filteredStart = filter (\ (st,mi,en) -> (st == start)) moves
        filteredEnd = filter (\ (st, mi, en) -> (en == end)) filteredStart
    in
        if (length filteredEnd) == 1 then
            Just (head filteredEnd)
        else
            Nothing

-- Consumes user input and recurses until a valid coordinate is entered
getCoordinate :: [Move] -> IO String
getCoordinate moves =
    do
        coordWithDel <- getLine
        let coord = fixdel coordWithDel
        if (coord == "Hint" || coord == "hint" || coord == "H" || coord == "h") then do
           putStrLn "Available moves:"
           putStrLn (show (map showMove moves))
           coord <- getCoordinate moves
           return coord
        else if not (checkValidCoord coord) then do
            let line = (coord ++ " is not a valid coordinate. Please try again:")
            putStrLn line
            coord <- getCoordinate moves
            return coord
        else return coord

-- Returns a new game state with the move performed
updateGameState :: State -> Move -> State
updateGameState (State board moves) move =
  let newBoard = performMove board move
      newMoves = generateValidMoves newBoard 
  in  State newBoard newMoves 

-- Performs the given move on the game state
-- Removes the peg from the Starting and Middle positions
-- Adds a peg to the end position
performMove :: Board -> Move -> Board 
performMove board (start, middle, end) =
  let b1 = modifyPoardPosition board start 0
      b2 = modifyPoardPosition b1 middle 0
  in modifyPoardPosition b2 end 1


-- Consumes a board, coordinate, and state
-- Returns a new board with the status set to newState
modifyPoardPosition :: Board -> Coordinate -> Int -> Board
modifyPoardPosition board coord newState =
  let row = getRowNumber coord 
      index = case findIndex (matchEntry coord) (board!!(row+2)) of
              Just index -> index
              Nothing -> 0
      (start,elem:rest) = splitAt index (board!!(row+2)) -- Split the row 
      newRow = start ++ ((fst elem), newState) : rest -- Modify the row and splice it together
      (firstRows,_:restRows) = splitAt (row+2) board -- Split the list of rows
  in  firstRows ++ [newRow] ++ restRows -- Replace the old row with the new row


------- Generating lists of moves -------
-- Generates all legal moves for a given board
-- A legal move is a any move that is possible for the given board
generateLegalMoves :: Board -> [Move]
generateLegalMoves startingBoard =
  do
    let arr = [legalMoves row col startingBoard | row <- [2..8], col <- [2..8]]
    let processed = concat [[fromJust x | x <- row, isJust x] | row <- arr, row/=[]]
    return processed!!0

legalMoves :: Int -> Int -> Board -> [Maybe Move]
legalMoves row col board
  | snd (board!!row!!col) == -1 = []
  | otherwise = [legalMovesHelper row col board x | x <- ["up", "right", "down", "left"]]

legalMovesHelper :: Int -> Int -> Board -> String -> Maybe Move
legalMovesHelper row col board dir
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


-- Generates all valid moves for a given board
-- A position is valid if:
    -- Starting position == 1
    -- Middle position == 1
    -- Ending position == 0
generateValidMoves :: Board -> [Move]
generateValidMoves board =
  let legal = generateLegalMoves startingBoard
   in filter (\(start, mid, end) -> 
        let startEntry = getBoardEntry board start
            midEntry = getBoardEntry board mid
            endEntry = getBoardEntry board end
        in isJust startEntry && isJust midEntry && isJust endEntry -- check that the coord exists
           && snd (fromJust startEntry) == 1 -- a peg must be in the starting hole
           && snd (fromJust midEntry) == 1 -- there must be a peg to jump over
           && snd (fromJust endEntry) == 0) -- there must be a spot for the peg to land
           legal 

-- Takes a game board and a coordinate, returns BoardEntry for that coordinate 
getBoardEntry :: Board -> Coordinate -> Maybe BoardEntry 
getBoardEntry board coord =
    let row = (fromEnum (head coord))-65
    in find (matchEntry coord) (board!!(row+2))



-------- Display Functions --------
-- Prints the board to the console in a readable format
displayBoard :: Board -> IO()
displayBoard board =
  do
     let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))]
     putStrLn "   1 2 3 4 5 6 7"
     putStrLn (foldl (\acc (l, row) -> acc ++ l ++ "  " ++ row ++ "\n")
                     ""
                     (zip [['A'],['B'],['C'],['D'],['E'],['F'],['G']]
                          [(unwords [if y == -1
                                      then " "
                                        else if y == 1
                                                then "."
                                                  else "o"
                                                  | (x,y) <- borderless!!row]) | row<-[0..6]]))

-- Displays the move on the board
-- Prints to console in a readable format
displayMove :: Board -> Move -> IO ()
displayMove board (start, middle, end) =
  do
     let
         borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))]
     putStrLn "   1 2 3 4 5 6 7"
     putStrLn (foldl (\acc (l, row) -> acc ++ l ++ "  " ++ row ++ "\n")
                     ""
                     (zip [['A'],['B'],['C'],['D'],['E'],['F'],['G']]
                          [(unwords [if y == -1
                                      then " "
                                        else if (x==start || x==middle)
                                                then "x"
                                             else if (x==end)
                                                then "*"
                                             else if y == 1
                                                then "."
                                                  else "o"
                                                  | (x,y) <- borderless!!row]) | row<-[0..6]]))

-- Displays a list of moves from a given state
displayAllMoves :: State -> [Move] -> IO ()
displayAllMoves state@(State bd _) moves =
    do
        displayBoard bd
        foldM_ (\acc_state@(State acc_bd _) move ->
                    do
                        putStrLn ("Move: " ++ (showMove move) ++ "\n")
                        displayMove (getBoard acc_state) move
                        putStrLn "      |  |  |       "
                        putStrLn "      V  V  V       "
                        putStrLn "                    "
                        let new_state = updateGameState acc_state move
                        displayBoard (getBoard new_state)
                        return new_state) state moves
        putStrLn "Found solution!"


--------- Helper Functions ---------
showMove :: Move -> String
showMove (start,mid,end) = start ++ " to " ++ end

getBoard :: State -> Board
getBoard (State board _) = board

checkValidMove :: State -> Move -> Bool
checkValidMove (State bd mvs) mv = elem mv mvs

-- Used to fix the input from getLine
fixdel :: String -> String
fixdel lst = foldl (\acc x -> if (x=='\DEL')
                                then (if (length acc > 0) then init acc else acc)
                              else (acc++[x])) [] lst


-- Consumes a coordinate and returns true if it is a valid coordinate on the board
-- Depends on startingBoard being the same shape as the game board...
checkValidCoord :: String -> Bool
checkValidCoord coord =
    let flattenedBoard = concat startingBoard in
    length (filter (\ entry -> (fst entry) == coord) flattenedBoard) == 1

-- Consumes a coordinate and returns the corresponding row number
-- Does not accounting for padding in the game board
getRowNumber :: Coordinate -> Int
getRowNumber coord = (fromEnum (head coord)) - fromEnum('A')

-- Return true if the coordiante matches the coordiante in the board entry
matchEntry :: Coordinate -> BoardEntry -> Bool
matchEntry coord entry = fst entry == coord

-- Only one position left in the board with a filled peg
win :: State -> Bool
win (State board moves) =
   let flattenedBoard = concat board in
   foldr (\ entry acc ->
        if (fst entry == "D4") then
            acc && (snd entry == 1)
        else
            acc && (snd entry /= 1)
   ) True flattenedBoard

-- No moves left to make
lose :: State -> Bool
lose (State board moves) =
    (length moves) == 0

----------------------- SOLVER(s) ----------------------------
-- A DFS search that uses a HashTreeDict to memoize results
search :: State -> Mem -> Int -> ((Bool, [Move]), Mem)
search (State board moves) memory depth
    | depth == 0 = ((False, []), memory)
    | isJust memo =
        --trace("fount cg!")
        --trace("num pegs: " ++ show (length (filter (\entry -> (snd entry) == 1) (concat board))))
        trace(("Number of boards tried: " ++ show (length (tolist memory))))
        (fromJust memo, memory)
    | win (State board moves) = ((True, []), memory)
    | lose (State board moves) = ((False, []), memory)
    | otherwise = foldl tryMove ((False, []), memory) moves
    where
        hash = hashBoard board
        memo = getval hash memory
        tryMove ((found, mvs), newMem) move =
            if found then ((found, mvs), newMem) else
            let newState = updateGameState (State board moves) move
                ((result, newMoves), rMem) = search newState newMem (depth - 1)
                finalMem = insertval hash (result, newMoves) rMem
            in if result then ((True, (move:newMoves)), finalMem) else ((found, moves),finalMem)

-- Here we have a few different solvers that we can use to solve the game
-- A DFS solver
nateSolve :: State -> [Move] -> [Int] -> (Bool, [Move], [Int])
nateSolve (State bd moves) movesPlayed prevFails =
  if (onePegLeft (State bd moves)) then
    if win (State bd moves) then
      trace("found winning board!")
      (True, movesPlayed, prevFails)
    else
      (False, movesPlayed, prevFails)
  else
    if elem (hashBoard bd) prevFails then
     -- trace("found congruent!")
      --trace("num pegs: " ++ show (length (filter (\entry -> (snd entry) == 1) (concat bd))))
      trace("saved boards: " ++ show (length prevFails))
      (False, movesPlayed, prevFails)
    else
      let statesAndMoves = newStatesAndMoves (State bd moves) movesPlayed prevFails in
        foldl recurse (False,movesPlayed,prevFails) statesAndMoves

recurse :: (Bool, [Move], [Int]) -> (State, [Move], [Int]) -> (Bool, [Move], [Int])
recurse dflt new =
  if (first dflt) then dflt
  else let
      result = nateSolve (first new) (second new) (third dflt)
      hashed_bd = hashBoard (getBoard (first new))
      new_fails = hashed_bd:(third result)
      updated_result = if (first result)
                             then (True, (second result), (nub(new_fails++(third dflt))))
                             else ((first dflt), (second dflt), (nub(new_fails++(third dflt))))
      in
        updated_result

        where first  (a,_,_) = a
              second (_,a,_) = a
              third  (_,_,a) = a

newStatesAndMoves :: State -> [Move] -> [Int] -> [(State, [Move], [Int])]
newStatesAndMoves (State bd moves) movesPlayed prevFails =
  map (\move -> ((updateGameState (State bd moves) move), (movesPlayed++[move]), prevFails)) moves

-- Only one position left in the board with a filled peg
onePegLeft :: State -> Bool
onePegLeft (State board moves) =
  let flattenedBoard = concat board in
  length (filter (\entry -> (snd entry) == 1) flattenedBoard) == 1

-- A search that uses a priority queue and a heuristic to guide the search
heuristicSearch :: Pq -> Mem -> (Bool, [Move], Mem, Pq)
heuristicSearch currentQueue memory =
    let maybeState = PQ.getMin currentQueue
        queue = PQ.deleteMin currentQueue in
    case maybeState of
        Nothing -> (False, [], memory, queue)
        Just currentState ->
            let (_, (currState@(State board avail), played)) = currentState in
            if (win currState) then
                trace("won game!")
                (True, played, memory, queue)
            else if (lose currState) then
                trace("lost game :(")
                (False, [], memory, queue)
            else let
                hash = hashBoard board
                memo = getval hash memory in
                case memo of
                   Just (bool, move) ->
                       --trace("hash is" ++ show hash)
                       trace("Number of boards tried: " ++ show (length (tolist memory)))
                       (bool, [], memory, queue)
                   Nothing ->
                        let newStates = map (\ move -> (updateGameState currState move, move)) avail
                            newValues = map (\ (st@(State newBoard newAvail), move) -> ((heuristicValue newBoard), (st, (played ++ [move])))) newStates
                            queueWithNewStates = foldl (\ pq val -> PQ.insert val pq) queue newValues
                            (res, rPlayed, rMem, rPQ) = heuristicSearch queueWithNewStates memory in
                            (case res of
                                True -> (True, rPlayed, rMem, rPQ)
                                False -> heuristicSearch queue (insertval hash (False, []) rMem))
solve_h state = heuristicSearch (PQ.singleton (0, (state, []))) emptyDict

-- Manhattan distance
heuristicValue :: Board -> Int
heuristicValue board = sum [distanceToCenter coord | (coord, status) <- concat (drop 2 (take (length board - 2) board)), status == 1]
  where
    numRows = length board
    numCols = length $ head board
    centerRow = (numRows `div` 2) + 1
    centerCol = (numCols `div` 2) + 1
    distanceToCenter :: Coordinate -> Int
    distanceToCenter coord = (row - centerRow) ^ 2 + (col - centerCol) ^ 2
      where
        row = ord (head coord) - 64
        col = read (tail coord) :: Int



-- Hash must return the same value for rotationally symmetric boards
-- Hashing technique taken from https://ece.uwaterloo.ca/~dwharder/aads/Algorithms/Backtracking/Peg_solitaire/
hashBoard :: Board -> Int
hashBoard board =
   let tlbrr = foldl (\acc x -> (2*acc) + x) 0 (top_left_bottom_right_right board)
       tlbrd = foldl (\acc x -> (2*acc) + x) 0 (top_left_bottom_right_down board)
       trbld = foldl (\acc x -> (2*acc) + x) 0 (top_right_bottom_left_down board)
       trbll = foldl (\acc x -> (2*acc) + x) 0 (top_right_bottom_left_left board)
       brtll = foldl (\acc x -> (2*acc) + x) 0 (bottom_right_top_left_left board)
       brtlu = foldl (\acc x -> (2*acc) + x) 0 (bottom_right_top_left_up board)
       bltru = foldl (\acc x -> (2*acc) + x) 0 (bottom_left_top_right_up board)
       bltrr = foldl (\acc x -> (2*acc) + x) 0 (bottom_left_top_right_right board)
       minhash = minimum [tlbrr,tlbrd,trbld,trbll,brtll,brtlu,bltru,bltrr]
       in
--         trace("tlbrr: "++ showIntAtBase 2 intToDigit tlbrr "")
--         trace("real : 11100001010011010111101101111111100111000011100 \n")
--
--         trace("tlbrd: "++ showIntAtBase 2 intToDigit tlbrd "")
--         trace("real : 11100001110011001111011111110111100101000011100 \n")
--
--         trace("trbld: "++ showIntAtBase 2 intToDigit trbld "")
--         trace("real : 11100001010011011111011111110011100111000011100 \n")
--
--         trace("trbll: "++ showIntAtBase 2 intToDigit trbll "")
--         trace("real : 11100001010011010111011011111111100111000011100 \n")
--
--         trace("brtll: "++ showIntAtBase 2 intToDigit brtll "")
--         trace("real : 11100001110011111111011011110101100101000011100 \n")
--
--         trace("brtlu: "++ showIntAtBase 2 intToDigit brtlu "")
--         trace("real : 11100001010011110111111101111001100111000011100 \n")
--
--         trace("bltru: "++ showIntAtBase 2 intToDigit bltru "")
--         trace("real : 11100001110011100111111101111101100101000011100 \n")
--
--         trace("bltrr: "++ showIntAtBase 2 intToDigit bltrr "")
--         trace("real : 11100001110011111111101101110101100101000011100 \n")
--
--         trace("min : "++ showIntAtBase 2 intToDigit min "")
--         trace("real: 11100001010011010111011011111111100111000011100 \n")
         minhash


-- The following 8 hash functions correspond with the 8 congruent board states outlined at https://ece.uwaterloo.ca/~dwharder/aads/Algorithms/Backtracking/Peg_solitaire/ 
top_left_bottom_right_right :: Board -> [Int]
top_left_bottom_right_right board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | row <- [0..6], col <- [0..6]]

top_left_bottom_right_down :: Board -> [Int]
top_left_bottom_right_down board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | col <- [0..6],
                                                       row <- [0..6]]

top_right_bottom_left_left :: Board -> [Int]
top_right_bottom_left_left board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | row <- [0..6],
                                                       col <- (foldl (flip (:)) [] [0..6])]

top_right_bottom_left_down :: Board -> [Int]
top_right_bottom_left_down board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | col <- (foldl (flip (:)) [] [0..6]),
                                                       row <- [0..6]]

bottom_right_top_left_left :: Board -> [Int]
bottom_right_top_left_left board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | row <- (foldl (flip (:))) [] [0..6],
                                                       col <- (foldl (flip (:)) [] [0..6])]

bottom_right_top_left_up :: Board -> [Int]
bottom_right_top_left_up board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | col <- (foldl (flip (:)) [] [0..6]),
                                                       row <- (foldl (flip (:))) [] [0..6]]

bottom_left_top_right_up :: Board -> [Int]
bottom_left_top_right_up board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | col <- [0..6],
                                                       row <- (foldl (flip (:))) [] [0..6]]

bottom_left_top_right_right :: Board -> [Int]
bottom_left_top_right_right board =
  let borderless = [init (init (drop 2 row)) | row <- (init (init (drop 2 board)))] in
    [if (snd(borderless!!row!!col))==0 then 1 else 0 | row <- (foldl (flip (:))) [] [0..6],
                                                       col <- [0..6]]