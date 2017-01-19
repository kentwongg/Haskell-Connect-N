-- CPSC 449 FALL 2016
-- By Kent Wong 

-- pad the board with Nothing - DONE
-- move into a column that doesnt exist is not allowed - DONE
-- move into full column does not happen- DONE
-- a move that works - DONE
-- a board that is not in a win state will never say win - DONE
-- a board that is in win state will say win - DONE

--p2


main = undefined





-- Gallagher assigned functions

makeMove :: BoardState -> Int -> Piece -> Maybe BoardState
--makeMove b intX p = if ((isFull b intX) && (isOutBounds b intX)) then Nothing else Just (updateBoard b intX p) 
makeMove b intX p = if (checkIllegal b intX) then Nothing else Just (updateBoard b intX p) 


checkWin :: BoardState -> Maybe Piece
checkWin b = case (lastMove b) of
    Red -> 
        -- did red win with last move?
        if (listHasAWinner (columns b) (Just Red) (numToConnect b)) then (Just Red)
        else if (listHasAWinner (rows b) (Just Red) (numToConnect b)) then (Just Red)
        else if (listHasAWinner (diagonalsForward b) (Just Red) (numToConnect b)) then (Just Red)
        else if (listHasAWinner (diagonalsBackward b) (Just Red) (numToConnect b)) then (Just Red)
        -- did yell win on prev turn?
        else if (listHasAWinner (columns b) (Just Yellow) (numToConnect b)) then (Just Yellow)
        else if (listHasAWinner (rows b) (Just Yellow) (numToConnect b)) then (Just Yellow)
        else if (listHasAWinner (diagonalsForward b) (Just Yellow) (numToConnect b)) then (Just Yellow)
        else if (listHasAWinner (diagonalsBackward b) (Just Yellow) (numToConnect b)) then (Just Yellow) 
        -- no winners
        else Nothing
    Yellow ->
        -- did yell win with last move?
        if (listHasAWinner (columns b) (Just Yellow) (numToConnect b)) then (Just Yellow)
        else if (listHasAWinner (rows b) (Just Yellow) (numToConnect b)) then (Just Yellow)
        else if (listHasAWinner (diagonalsForward b) (Just Yellow) (numToConnect b)) then (Just Yellow)
        else if (listHasAWinner (diagonalsBackward b) (Just Yellow) (numToConnect b)) then (Just Yellow) 
        -- did red win on prev turn?
        else if (listHasAWinner (columns b) (Just Red) (numToConnect b)) then (Just Red)
        else if (listHasAWinner (rows b) (Just Red) (numToConnect b)) then (Just Red)
        else if (listHasAWinner (diagonalsForward b) (Just Red) (numToConnect b)) then (Just Red)
        else if (listHasAWinner (diagonalsBackward b) (Just Red) (numToConnect b)) then (Just Red)
        -- Nothing Won
        else Nothing



-- Part 3 define the following functions

columns :: BoardState -> [[Maybe Piece]]
columns b = map (fillNothing (numRows b)) (map(map Just) (theBoard(b))) -- this will use fillNothing to first pad the numrows B as args, then map map Just(list of lists) to all pieces into Just Yellow or Just Red
-- basically an entire board of just pieces

-- return a list of lists (rows) from board
rows :: BoardState -> [[Maybe Piece]]
rows b = reverse (transpose(columns b))

-- find diagonal for the forward
diagonalsForward :: BoardState -> [[Maybe Piece]]
diagonalsForward b = calculateDiagonals (rows b)

-- find diagonal for the backward
diagonalsBackward :: BoardState -> [[Maybe Piece]]
diagonalsBackward b = calculateDiagonals (reverse(rows b)) 


-- custom functions start here



-- please see Design Document for full explanation of Calculate Diagonals
-- now recursively call through the bottom
-- Implements method via below source
-- source: http://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell (Post by Tobias Weck)

calculateDiagonals :: [[Maybe Piece]] -> [[Maybe Piece]]
calculateDiagonals [] = [] -- nop
calculateDiagonals ([]:xs) = xs -- append empty list to tail ; this base case tells it it stop
calculateDiagonals list    = zipWith (++) (map (:[]) (map head list) ++ repeat []) ([]:(calculateDiagonals (map tail list))) -- zipwith is like a zipper. will lista and listb and passes them into a function
-- and generates a new list
-- map :[]  this wraps 
-- head pops off the head of a list
-- repeat


-- fill nothing works as follows. Take in a count like the args, then start recursively calling fill nothing. if its 0 then do nothing
-- else continue to append nothing and continue calling fillNothing while reducing count
-- will fill up the board with nothing
fillNothing :: Int -> [Maybe Piece] -> [Maybe Piece]
fillNothing leCount (x:xs) =  x : fillNothing (leCount-1) xs --
fillNothing 0 [] = [] -- base case is nothing
fillNothing leCount [] = Nothing : fillNothing (leCount-1) []  -- base case if there is a count left, go put nothing at head, then recursive call


-- returns a list of list. 
-- map head goes inside and applies head 
-- head by itself takes the head off of a list
-- map goes through all of it 
transpose :: [[Maybe Piece]] -> [[Maybe Piece]]
transpose ([]:xs) = []
transpose list = ((map head list):[]) ++ transpose (map tail list)

-- simplified and combined version of isfull and isoutbounds
checkIllegal :: BoardState -> Int -> Bool
checkIllegal b intX = ( (intX <= 0) || ((numColumns b) < intX) || ( (length((theBoard b) !! (intX-1)) >= (numRows b))))
-- is it out of bounds? is the damn thing full? then not true...its legal....horray

isFull :: BoardState -> Int -> Bool -- checks if a column is full
isFull b intX = (length((theBoard b) !! (intX-1)) == (numRows b)) -- indexes and gets that particular column 

-- old function that would be called for safe checking if its trying to drop into nothing
isOutBounds :: BoardState -> Int -> Bool -- checks if a choice is out of bounds. Just check if the record column
isOutBounds b intX = if ((numColumns b) >= intX)
                        then False
                        else True 

-- once we finish safety checks, actually update the board
updateBoard :: BoardState -> Int -> Piece -> BoardState -- update the board state given an Int
updateBoard b intX piece = b {
                                theBoard = (take (intX-1) (theBoard b)) ++ [(((theBoard b) !! (intX-1)) ++ [piece])] ++ (drop intX (theBoard b)),
                                lastMove = piece
                                -- take the length arg, get record theboard (list of lists) then catenate with the index as arg of the list of lists (SO just that one list) + then cat again with the drop end
                                -- abc = a+updatedb+c                                
                            }

-- list has a winner is a boolean at the end....iterate through entire board to find that one true love/list (list of pieces)
listHasAWinner :: [[Maybe Piece]] -> Maybe Piece -> Int -> Bool
listHasAWinner b whatPiece numToConnect
    | (null b) = False
    | (championList whatPiece (head b) 0 numToConnect) = True
    | otherwise = listHasAWinner (tail b) whatPiece numToConnect


-- takes a piece and reverses it
flipPiece :: Piece -> Piece -- reverses a piece
flipPiece p = 
        if (p == Yellow) 
                  then (Red)
                  else (Yellow) 


data Piece = Red | Yellow -- 2players, yell, red. these represent a piece in the board
    deriving (Eq)

type Column = [Piece]  -- alias
-- type Board = [Column]

type Board = [Column]
-- this is only for example for a 6x7 board
--bdFig1 =
--    let
--        c1 = []
--        c2 = [Red,Yellow]
--        c3 = [Yellow]
--        c4 = [Red,Red]
--        c5 = [Red,Red,Yellow]
--        c6 = [Yellow,Yellow]
--        c7 = []
--    in
--        [c1,c2,c3,c4,c5,c6,c7]
 
 -- iterates through and considers if a winning list has occurred
championList :: Maybe Piece -> [Maybe Piece] -> Int -> Int -> Bool
championList whatPiece list leCount numToConnect 
    | numToConnect == 0 = True 
    | (null list) = False
    | (head list) == whatPiece = (if ((leCount+1) == numToConnect) then True else (championList whatPiece (tail list) (leCount+1) numToConnect))
    | null (tail list) = False 
    | otherwise = championList whatPiece (tail list) 0 numToConnect   -- else case


-- data type with the records
data BoardState = BS
    {theBoard :: Board, -- list of lists, see above
    lastMove :: Piece, -- red or yellow
    numColumns :: Int,
    numRows :: Int,
    numToConnect :: Int} -- num to connect (n)
    --deriving (Show) -- COMMENT ME OUT

-- old dropPiece to test basic functinaklity
dropPiece :: Column -> Piece -> Maybe Column
dropPiece [] p = Nothing
dropPiece l p = Just (p:l) -- new pieces are the head and it grows up
       
