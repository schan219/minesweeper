module Minesweeper where

--A move is a tuple of coordinates on the grid
type AMove = (Int, Int)
--the game state is a list of the moves played so far
type State = [AMove]

--There are two possible actions:
--i.) make a move by passing the move (of type AMove), the current game state (of type State), mine locations, and hints list to the Move function
--ii.) start the game
data Action = Move AMove State State [[Int]]
            | Start

data Result = EndOfGame Int -- end of game
            | ContinueGame State [AMove] [AMove] [[Int]] --continue game by passing new state (of type State),
                                                         --list of possible moves (of type [AMove]),
                                                         --list of moves that step onto mines,
                                                         --and list of lists of hint numbers to the ContinueGame function
        deriving (Eq, Show) --this makes Result equatable and printable

--Game is a function that takes an Action and returns a Result
type Game = Action -> Result

-------- Minesweeper ---------

--Minesweeper is a Game
minesweeper :: Game
--takes (Move (x,y) state) because Game takes an Action, and Action is Move AMove State
--move is a tuple of coordinates (i.e. AMove)
--state is a list of AMove (i.e. State)
minesweeper (Move move state mines hints)
    | win move state mines = EndOfGame 1 --game is won
    | lose move mines = EndOfGame 0 --game is lost
    --the list comprehension gets all tuples in the grid not yet selected
    | otherwise = ContinueGame (move:state) [act | act <- (cross [0..9] [0..9]), not (act `elem` (move:state))] mines (reveal move mines hints)

--initialise a game
--at this point, no moves have been played and all coordinate pairs in the grid are viable moves
--list of moves that step onto mines is also initialised here, in the third argument; for now the diagonal is all mines
--list of hints is all -1 because all are hidden
minesweeper Start = ContinueGame [] (cross [0..9] [0..9]) [(x,y) | (x,y) <- (cross [0..9] [0..9]), x == y] z
    where z = [[-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1],
              [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1]]

--game is won if the move being made and the moves played so far reveal all cells other than the ones that are mines
win move state mines = move:state == [good | good <- (cross [0..9] [0..9]), not (good `elem` mines)]

--game is lost if the move being made steps on a mine
lose move mines = move `elem` mines

--reveal the cells and surrounding cells
reveal move mines hints = hints --temporarily just returning same hints

cross [] lst2 = [];
cross lst1 [] = [];
cross (h1:t1) (h2:t2) = ((h1,h2):(cross [h1] t2)) ++ (cross t1 (h2:t2))
