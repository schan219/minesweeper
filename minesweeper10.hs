module Minesweeper where

--A move is a tuple of coordinates on the grid
type AMove = (Int, Int)
--the game state is a list of the moves played so far
type State = [AMove]

--There are two possible actions:
--i.) make a move by passing the move (of type AMove), the current game state (of type State), mine locations, and hints list to the Move function
--ii.) start the game
data Action = Move AMove State [AMove] [[(AMove,Int)]]
            | Start

data Result = EndOfGame Int -- end of game
            | ContinueGame State [AMove] [AMove] [[(AMove,Int)]] --continue game by passing new state (of type State),
                                                         --list of possible moves (of type [AMove]),
                                                         --list of moves that step onto mines,
                                                         --and list of lists of hint numbers to the ContinueGame function
        deriving (Eq, Show) --this makes Result equatable and printable

--Game is a function that takes an Action and returns a Result
type Game = Action -> Result

-------- Minesweeper ---------

--Minesweeper is a Game
minesweeper :: Game
--takes (Move move state mines hints) because Game takes an Action, and Action is Move AMove State [AMove] [[(AMove,Int)]]
--move is a tuple of coordinates (i.e. AMove)
--state is a list of AMove (i.e. State)
minesweeper (Move move state mines hints)
    | win move state mines = EndOfGame 1 --game is won
    | lose move mines = EndOfGame 0 --game is lost
    --the list comprehension gets all tuples in the grid not yet selected
    | otherwise = ContinueGame ((moves_from_reveal move mines hints [])++state) [act | act <- (cross [0..8] [0..8]), not (act `elem` (move:state))] mines (reveal move mines hints [])

--THIS IS WHAT'S BROKE!!!
moves_from_reveal move mines hints locked = let list_of_elements = flatten hints
                                                --neighbours are elements in list_of_elements that immediately surround the element at (x,y)
                                                neighbours = [((a,b),v) | ((a,b),v) <- list_of_elements, a `elem` [x-1..x+1], b `elem` [y-1..y+1], a > -1, a < 9, b > -1, b < 9]
                                                --cleans are neighbours that do not have mines
                                                cleans = [(k,v) | (k,v) <- neighbours, not (k `elem` mines)]
                                                --subbed is just a way to refer to cell_subbed after sub is complete
                                                subbed = sub_grid hints (x,y) (num_mines_around (x,y) mines)
                                                --list of neighbours that are in same row or column as a mine in the neighbourhood
                                                new_locked = [((a,b),v) | ((a,b),v) <- neighbours, ((((a-1),b) `elem` mines) && (((a-1),b),-1) `elem` neighbours) ||
                                                                                                   ((((a-2),b) `elem` mines) && (((a-2),b),-1) `elem` neighbours) ||
                                                                                                   ((((a+1),b) `elem` mines) && (((a+1),b),-1) `elem` neighbours) ||
                                                                                                   ((((a+2),b) `elem` mines) && (((a+2),b),-1) `elem` neighbours) ||
                                                                                                   (((a,(b-1)) `elem` mines) && ((a,(b-1)),-1) `elem` neighbours) ||
                                                                                                   (((a,(b-2)) `elem` mines) && ((a,(b-2)),-1) `elem` neighbours) ||
                                                                                                   (((a,(b+1)) `elem` mines) && ((a,(b+1)),-1) `elem` neighbours) ||
                                                                                                   (((a,(b+2)) `elem` mines) && ((a,(b+2)),-1) `elem` neighbours)] ++ locked
  
                                                --clean_closed_unlocked consists of elements of cleans that have not been revealed already and are not in new_locked
                                                clean_closed_unlocked = [((a,b),v) | ((a,b),v) <- cleans, v == -1, (not (((a,b),v) `elem` new_locked))]
                                            in
                                                --recursively call reveal on each element in clean_closed, passing subbed in place of hints (since its the new hints list)
                                                foldr (\c subbed -> moves_from_reveal (fst c) mines subbed new_locked) subbed clean_closed_unlocked



--initialise a game
--at this point, no moves have been played and all coordinate pairs in the grid are viable moves
--list of moves that step onto mines is also initialised here, in the third argument; for now the diagonal is all mines
--list of hints is all -1 because all are hidden
minesweeper Start = ContinueGame [] (cross [0..8] [0..8]) [(x,y) | (x,y) <- (cross [0..8] [0..8]), x == y] z
                    where z = [[((0,0),-1),((0,1),-1), ((0,2),-1), ((0,3),-1), ((0,4),-1), ((0,5),-1), ((0,6),-1), ((0,7),-1), ((0,8),-1)],
                               [((1,0),-1),((1,1),-1), ((1,2),-1), ((1,3),-1), ((1,4),-1), ((1,5),-1), ((1,6),-1), ((1,7),-1), ((1,8),-1)],
                               [((2,0),-1),((2,1),-1), ((2,2),-1), ((2,3),-1), ((2,4),-1), ((2,5),-1), ((2,6),-1), ((2,7),-1), ((2,8),-1)],
                               [((3,0),-1),((3,1),-1), ((3,2),-1), ((3,3),-1), ((3,4),-1), ((3,5),-1), ((3,6),-1), ((3,7),-1), ((3,8),-1)],
                               [((4,0),-1),((4,1),-1), ((4,2),-1), ((4,3),-1), ((4,4),-1), ((4,5),-1), ((4,6),-1), ((4,7),-1), ((4,8),-1)],
                               [((5,0),-1),((5,1),-1), ((5,2),-1), ((5,3),-1), ((5,4),-1), ((5,5),-1), ((5,6),-1), ((5,7),-1), ((5,8),-1)],
                               [((6,0),-1),((6,1),-1), ((6,2),-1), ((6,3),-1), ((6,4),-1), ((6,5),-1), ((6,6),-1), ((6,7),-1), ((6,8),-1)],
                               [((7,0),-1),((7,1),-1), ((7,2),-1), ((7,3),-1), ((7,4),-1), ((7,5),-1), ((7,6),-1), ((7,7),-1), ((7,8),-1)],
                               [((8,0),-1),((8,1),-1), ((8,2),-1), ((8,3),-1), ((8,4),-1), ((8,5),-1), ((8,6),-1), ((8,7),-1), ((8,8),-1)]]

--game is won if the move being made and the moves played so far reveal all cells other than the ones that are mines
win move state mines = move:state == [good | good <- (cross [0..8] [0..8]), not (good `elem` mines)]

--game is lost if the move being made steps on a mine
lose move mines = move `elem` mines

--reveal the cell and surrounding cells
                           --create a single, flat list of all elements in hints called list_of_elements
reveal :: AMove -> [AMove] -> [[(AMove, Int)]] -> [(AMove, Int)] -> [[(AMove, Int)]]
reveal (x,y) mines hints locked = let list_of_elements = flatten hints
                                      --neighbours are elements in list_of_elements that immediately surround the element at (x,y)
                                      neighbours = [((a,b),v) | ((a,b),v) <- list_of_elements, a `elem` [x-1..x+1], b `elem` [y-1..y+1], a > -1, a < 9, b > -1, b < 9]
                                      --cleans are neighbours that do not have mines
                                      cleans = [(k,v) | (k,v) <- neighbours, not (k `elem` mines)]
                                      --subbed is just a way to refer to cell_subbed after sub is complete
                                      subbed = sub_grid hints (x,y) (num_mines_around (x,y) mines)
                                      --list of neighbours that are in same row or column as a mine in the neighbourhood
                                      new_locked = [((a,b),v) | ((a,b),v) <- neighbours, ((((a-1),b) `elem` mines) && (((a-1),b),-1) `elem` neighbours) ||
                                                                                         ((((a-2),b) `elem` mines) && (((a-2),b),-1) `elem` neighbours) ||
                                                                                         ((((a+1),b) `elem` mines) && (((a+1),b),-1) `elem` neighbours) ||
                                                                                         ((((a+2),b) `elem` mines) && (((a+2),b),-1) `elem` neighbours) ||
                                                                                         (((a,(b-1)) `elem` mines) && ((a,(b-1)),-1) `elem` neighbours) ||
                                                                                         (((a,(b-2)) `elem` mines) && ((a,(b-2)),-1) `elem` neighbours) ||
                                                                                         (((a,(b+1)) `elem` mines) && ((a,(b+1)),-1) `elem` neighbours) ||
                                                                                         (((a,(b+2)) `elem` mines) && ((a,(b+2)),-1) `elem` neighbours)] ++ locked
  
                                      --clean_closed_unlocked consists of elements of cleans that have not been revealed already and are not in new_locked
                                      clean_closed_unlocked = [((a,b),v) | ((a,b),v) <- cleans, v == -1, (not (((a,b),v) `elem` new_locked))]
                                  in
                                      --recursively call reveal on each element in clean_closed, passing subbed in place of hints (since its the new hints list)
                                      foldr (\c subbed -> reveal (fst c) mines subbed new_locked) subbed clean_closed_unlocked

flatten :: [[t]] -> [t]
flatten hints = foldr (++) [] hints

sub_grid :: [[(AMove, Int)]] -> AMove -> Int -> [[(AMove, Int)]]
sub_grid hints replace_coords new_value = foldr (\row -> (:) (sub_row row replace_coords new_value)) [] hints

sub_row :: (Eq t1) => [(t1,t2)] -> t1 -> t2 -> [(t1,t2)]
sub_row row replace_coords new_value = foldr (\e -> if ((fst e) /= replace_coords) then (:) e else (:) (replace_coords, new_value)) [] row

num_mines_around (x,y) mines = length [(a,b) | (a,b) <- mines, a `elem` [(x-1)..(x+1)], b `elem` [(y-1)..(y+1)], a > -1, a < 9, b > -1, b < 9]

cross [] lst2 = [];
cross lst1 [] = [];
cross (h1:t1) (h2:t2) = ((h1,h2):(cross [h1] t2)) ++ (cross t1 (h2:t2))