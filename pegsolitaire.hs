import Data.Array
import Data.Maybe
import Data.List
import Data.Maybe (isJust)
import TreeDict
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


type Mem = Dict State Bool


-------- USAGE --------
-- To perform a move:
--    st = State starting_board (generate_valid_moves starting_board)
--    newSt = update_game_state st ("B4","C4","D4")
--    (show st and newSt to see difference)
-- To see valid moves for a board:
--    example_board = starting_board
--    generate_valid_moves starting_board

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


win_board :: Board
win_board =
  [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("A3",0),("A4",0),("A5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("B3",0),("B4",0),("B5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("C1",0),("C2",0),("C3",0),("C4",0),("C5",0),("C6",0),("C7",0),("",-1),("",-1)],
  [("",-1),("",-1),("D1",0),("D2",1),("D3",0),("D4",0),("D5",0),("D6",0),("D7",0),("",-1),("",-1)],
  [("",-1),("",-1),("E1",0),("E2",1),("E3",1),("E4",0),("E5",0),("E6",0),("E7",0),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("F3",0),("F4",0),("F5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("G3",0),("G4",0),("G5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]


almost_win_board :: Board
almost_win_board =
  [[("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("A3",0),("A4",0),("A5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("B3",0),("B4",0),("B5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("C1",0),("C2",0),("C3",0),("C4",0),("C5",0),("C6",0),("C7",0),("",-1),("",-1)],
  [("",-1),("",-1),("D1",0),("D2",1),("D3",1),("D4",0),("D5",0),("D6",0),("D7",0),("",-1),("",-1)],
  [("",-1),("",-1),("E1",0),("E2",0),("E3",0),("E4",0),("E5",0),("E6",0),("E7",0),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("F3",0),("F4",0),("F5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("G3",0),("G4",0),("G5",0),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)],
  [("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1),("",-1)]]

waterloo_board :: Board
waterloo_board =
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

almost_lose_board :: Board
almost_lose_board =
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
generate_valid_moves :: Board -> [Move]
-- For each board entry, try all 4 legal moves?
    -- Starting position == 1
    -- Middle position == 1
    -- Ending position == 0
generate_valid_moves board = 
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


startState = (State starting_board (generate_valid_moves starting_board))
playGame = play startState
playEndGameWin = play (State almost_win_board (generate_valid_moves almost_win_board))
playEndGameLose = play (State almost_lose_board (generate_valid_moves almost_lose_board))

endGameStateWin = (State almost_win_board (generate_valid_moves almost_win_board))
endGameStateLose = (State almost_lose_board (generate_valid_moves almost_lose_board))

play :: State -> IO ()
play (State bd moves) =
  do
    display_board bd
    move <- parse_move moves
    let newState = update_game_state (State bd moves) move
    if (win newState) then do
        display_board (get_board newState)
        putStrLn "You win!"
    else if (lose newState) then do
        display_board (get_board newState)
        putStrLn "You lose :("
    else do
        putStrLn "Please select one of the following options:"
        putStrLn "Continue - Hint - Solve"
        play newState


--solve :: State -> [Move] -> Mem -> Maybe ((State, [Move]), Mem)
--solve (State bd moves) moves_played memory =
--    let stored_result = getval bd memory in
--    if (isJust stored_result) then
--        Just (((State bd moves), fromJust stored_result), memory)
--    if (win (State bd moves)) then
--        Just (((State bd moves), moves_played), memory)
--    else if (lose (State bd moves)) then
--        Nothing
--    else
--        let states = map (\ move -> (update_game_state (State bd moves) move, (move:moves_played))) moves in
--        let results = map (\ state ->
--            let result = solve (fst (fst state)) (snd (fst state)) memory) states in
--            if (isJust result) then
--                let memory = insertval (fst (fst (fromJust result))) (snd (fst (fromJust result))) memory
--        foldr (\ result rest -> if isJust result then result else rest) Nothing results
--
--solve_helper (State bd moves) move moves_played memory =
--    let newMoves = (move:moves_played)
--        newState = update_game_state (State bd moves) move
--        newMemory =  insertval (get_board newState) newMoves in
--        solve

--solve :: State -> [Move] -> [(State, Move)] -> [State] -> Maybe ((State, [Move]))
--solve (State bd moves) played next_states visited =
--    let nextStatesNew = (map (\ move -> ((update_game_state (State bd moves) move), move)) moves)
--        nextStatesFiltered = (filter (\ (ns, _) -> notElem ns visited) nextStatesNew) ++ next_states in
--    if (win (State bd moves)) then
--        Just ((State bd moves), played)
--    else if (nextStatesFiltered == []) then
--        Nothing
--    else
--        let (nextStateTuple:restStates) = nextStatesFiltered
--            nextState = fst nextStateTuple
--            nextMove = snd nextStateTuple in
--           case  solve nextState (played ++ [nextMove]) restStates ((State bd moves):visited) of
--            Just solution -> Just solution
--            Nothing ->  solve (State bd moves) played restStates ((State bd moves):visited)
--
--solve :: State -> [Move] -> [(State, Move, [Move])] -> [State] -> (Bool, [Move])
--solve (State bd moves) played nextStates visited =
--    let newStates = map (\ move -> ((update_game_state (State bd moves) move), move)) moves
--        filteredStates = filter (\ (newState, _) -> notElem newState visited) newStates
--        statesWithPlayed = map (\ (s,m) -> (s,m,played)) filteredStates
--        nextStatesNew = statesWithPlayed ++ nextStates in
--    if elem (State bd moves) visited then
--            (False, played)
--        else if (win (State bd moves)) then
--             (True, played)
--        else if (nextStatesNew == []) then
--             (False, played)
--    else
--        let (nextStateTriple:restStates) = nextStatesNew
--            (nextState, nextMove, nextPlayed) = nextStateTriple in
--            solve nextState (nextPlayed ++ [nextMove]) restStates ((State bd moves):visited)


stateInVisited state visited = getval state visited

solve :: [(State, Move, [Move])] -> Mem -> (Bool, [Move], Mem)
solve frontier visited =
    if (frontier == []) then
            (False, [], visited)
    else
        let (newState@(State board moves), newMove, newPlayed) = head frontier in
        if (win newState) then
            (True, newPlayed, visited)
        else case stateInVisited newState visited of
            Just bool -> solve (tail frontier) visited
            Nothing ->
                    let newFrontierValues = map (\ move -> ((update_game_state newState move), move, (move:newPlayed))) moves
                        nextFrontier = newFrontierValues ++ frontier
                        nextVisited = insertval newState True visited
                        res = solve (tail nextFrontier) nextVisited in
                        case res of
                            (True, _, _) -> res
                            (False, _, rvisited) -> solve (tail nextFrontier) rvisited


test_frontier = map (\ move -> ((update_game_state startState move), move, (move:[]))) (generate_valid_moves (get_board startState))

----
--solve :: State -> [Move] -> Set.Set State -> Maybe ((State, [Move]))
--solve (State bd moves) played visited =
--    let nextStatesNew = (map (\ move -> ((update_game_state (State bd moves) move), move)) moves)
--        nextStatesFiltered = (filter (\ (ns, _) -> not (Set.member ns visited)) nextStatesNew) in
--    if (win (State bd moves)) then
--        Just ((State bd moves), played)
--    else if (nextStatesFiltered == []) then
--        Nothing
--    else
--        case solve_helper nextStatesFiltered played (Set.insert (State bd moves) visited) of
--            Just solution -> solution
--            Nothing -> Nothing
--
--
---- Calls solve on each nextState
----solve_helper :: [(State, Move)] -> [Move] -> Set.Set State -> Maybe (State, [Move])
--solve_helper nextStates played visited =
--    if (nextStates == []) then
--        Nothing
--    else
--        let (nextState, nextMove) = head nextStates
--            result = solve nextState (nextMove:played) visited in
--        if (isJust result) then
--            Just result
--        else
--            solve_helper (tail nextStates) played visited


--solve :: State -> [Move] -> Mem -> ((Bool, [Move]), Mem)
--solve (State bd moves) moves_played memory =
--    let hash = hash_board bd in
--    case (getval hash memory) of
--        Just result ->
--            (result, memory)
--        Nothing ->
--            if win (State bd moves) then
--                ((True, moves_played), memory)
--            else if lose (State bd moves) then
--                ((False, moves_played), memory)
--            else
--                let states_and_moves = new_states_and_moves (State bd moves) moves_played in
--                    foldl (\ dflt new -> recurse dflt new) ((False, moves_played), memory) states_and_moves
--
--recurse :: ((Bool, [Move]), Mem) -> (State, [Move]) -> ((Bool, [Move]), Mem)
--recurse dflt new =
--    let memory = snd dflt
--        (result, rMem) = solve (fst new) (snd new) memory
--        hash = hash_board (get_board (fst new))
--        newMemory = insertval hash result rMem
--        updatedResult = if fst result
--                            then ((True, (snd result)))
--                        else (fst dflt)
--        in (updatedResult, newMemory)
--
--
--new_states_and_moves :: State -> [Move] -> [(State, [Move])]
--new_states_and_moves (State bd moves) moves_played =
--    map (\move -> ((update_game_state (State bd moves) move), (move: moves_played))) moves


--search :: State -> Mem -> Int -> ((Bool, [Move]), Mem)
--search (State board moves) memory depth
--    | depth == 0 = ((False, []), memory)
--    | isJust memo = (fromJust memo, memory)
--    | win (State board moves) = ((True, []), memory)
--    | lose (State board moves) = ((False, []), memory)
--    | otherwise = foldl tryMove ((False, []), memory) moves
--    where
--        hash = hashState board
--        memo = getval hash memory
--        tryMove ((found, moves), newMem) move =
--            if found then ((found, moves), newMem) else
--            let newState = update_game_state (State board moves) move
--                ((result, newMoves), rMem) = search newState newMem (depth - 1)
--                finalMem = insertval hash (result, newMoves) rMem
--            in if result then ((True, move:newMoves), finalMem) else ((found, moves),finalMem)
--        hashState board =
--            hash (map snd $ filter (\(coord, status) -> status /= -1) $ concat board)
--                    where hash xs = foldl' hashStep 0 xs
--                          hashStep a b = 31 * a + b

rotate_board1 board
   | board == [] = []
   | otherwise = let (row:rows) = board in row : rotate_board1 rows

rotate_board2 board
    | board == [] = []
    | null (head board) = []
    | otherwise = (map head board) : rotate_board2(map tail board)

rotate_board3 board = reverse (rotate_board2 board)

rotate_board4 board
    | board == [] = []
    | otherwise = let (row:rows) = board in (reverse row) : rotate_board4 rows

rotate_board5 board = reverse (rotate_board4 board)

rotate_board6 = map reverse . transpose

rotate_board7 board = transpose $ rotate_board6 board

rotate_board8 board = transpose $ reverse (rotate_board4 board)

hash_functions board = [rotate_board1 board, rotate_board2 board, rotate_board3 board, rotate_board4 board, rotate_board5 board, rotate_board6 board, rotate_board7 board, rotate_board8 board]

--hash_functions board = [rotate_board1 board]

test_hash_boards = hash_functions win_board

display_hashed_boards = mapM_ display_board test_hash_boards

generate_hash :: Board -> Int
generate_hash board =
    let collapsed = concat board
        game_board = drop 22 (take (length collapsed - 22) collapsed)
        arr = (map (\ (coord, status) -> if (status == -1) then 0 else status) game_board) in
        foldl (\ num x -> num * 2 + x) 0 arr

hash_board board =
    let boards = hash_functions board
        hashes = map generate_hash boards in
        minimum hashes


parse_move :: [Move] -> IO Move
parse_move moves =
    do
        putStrLn (show moves)
        putStrLn "Please enter the starting coordinate of the move:"
        startCoord <- get_coordinate
        putStrLn "Please enter the ending coordinate of the move:"
        endCoord <- get_coordinate
        let move = get_move startCoord endCoord moves
        if isJust move then do
            return (fromJust move)
        else do
            let line = (startCoord ++ " to " ++ endCoord ++ " is not a valid move. Please try again:")
            putStrLn line
            parse_move moves

get_move :: String -> String -> [Move] -> Maybe Move
get_move start end  moves =
    let filtered_start = filter (\ (st,mi,en) -> (st == start)) moves
        filtered_end = filter (\ (st, mi, en) -> (en == end)) filtered_start
    in
        if (length filtered_end) == 1 then
            Just (head filtered_end)
        else
            Nothing


get_coordinate ::  IO String
get_coordinate =
    do
      coord <- getLine
      if not (check_valid_coord coord) then do
         let line = (coord ++ " is not a valid coordinate. Please try again:")
         putStrLn line
         coord <- get_coordinate
         return coord
      else return coord


display_board :: Board -> IO()
display_board board =
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


--get_move :: IO () -> Move
--get_move =

get_board :: State -> Board
get_board (State board _) = board

check_valid_move :: State -> Move -> Bool
check_valid_move (State bd mvs) mv = elem mv mvs

check_valid_coord :: String -> Bool
check_valid_coord coord =
    let flattenedBoard = concat starting_board in
    length (filter (\ entry -> (fst entry) == coord) flattenedBoard) == 1

-- Returns a new game state with the move performed
update_game_state :: State -> Move -> State
update_game_state (State board moves) move =
  let newBoard = perform_move board move
      newMoves = generate_valid_moves newBoard 
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
      (start,elem:rest) = splitAt index (board!!(row+2)) -- Split the row 
      newRow = start ++ ((fst elem), newState) : rest -- Modify the row and splice it together
      (firstRows,_:restRows) = splitAt (row+2) board -- Split the list of rows
  in  firstRows ++ [newRow] ++ restRows -- Replace the old row with the new row


-- Consumes a coordinate and returns the corresponding row number
-- Does not accounting for padding in the game board
get_row_number :: Coordinate -> Int
get_row_number coord = (fromEnum (head coord)) - fromEnum('A')

-- Return true if the coordiante matches the coordiante in the board entry
match_entry :: Coordinate -> BoardEntry -> Bool
match_entry coord entry = fst entry == coord

-- Only one position left in the board with a filled peg
--win :: State -> Bool
--win (State board moves) =
--  let flattenedBoard = concat board in
--  length (filter (\entry -> (snd entry) == 1) flattenedBoard) == 1

win :: State -> Bool
win (State board moves) =
   let flattenedBoard = concat board in
   foldr (\ entry acc ->
        if (fst entry == "D4") then
            acc && (snd entry == 1)
        else
            acc && (snd entry /= 1)
   ) True flattenedBoard


lose :: State -> Bool
lose (State board moves) =
    (length moves) == 0

-- Parse user input into a move
-- Validate that the move is allowed (legal and valid)
-- Update the game state