import System.IO (readFile)
import Prelude

-- przygotowanie typów danych
type Pos = (Int,Int)
data State = UNSOLVED | FILLED | NOTFILLED deriving (Eq, Show)
type Value = Maybe Int
type Square = (State, Value)
type Board = [[Square]]


-- załadowanie pliku
readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename
    let puzzle = read contents :: [String]
    return puzzle

-- zaladowanie planszy
loadBoard :: [String] -> Board
loadBoard = map (map extractValues)
    where
        --konwersja do Inta
        extractValues :: Char -> Square
        extractValues a     | a == '.' = (UNSOLVED, Nothing)
                            | otherwise = (UNSOLVED, Just(read [a]::Int )) 

-- liczba wierszy planszy
numRows :: Board -> Int
numRows = length 

--liczba kolumn planszy 
numColumns :: Board -> Int
numColumns = length . head 

-- zwrocenie pola planszy na pozycji 
getSquare :: Board -> Pos -> Square
getSquare board (a, b) = board !! b !! a

-- zmienienie wartosci planszy, ix - kolumna, iy - wiersz
-- wywolanie updateSquare (1,1) (\x -> 4) macierz
updateSquare :: Pos -> (a->a) -> [[a]] -> [[a]]
updateSquare (ix, iy) m = updateAt iy (\row -> updateAt ix m row)
    where 
        --zmienienie wartosci listy w dowolnym miejscu
        updateAt :: Int -> (a->a) -> [a] -> [a]
        updateAt i m xs = let (prev, next) = splitAt i xs
                                        in prev ++ updateHead m next
                    
        --zmienianie wartosci glowy listy
        updateHead :: (a->a) -> [a] -> [a]
        updateHead _ [] = []
        updateHead m (x:xs) = m x : xs

updateSquareState :: Board -> Pos -> State -> Board
updateSquareState board (ix,iy) s = updateSquare (ix,iy) (const (s, snd (getSquare board (ix,iy)))) board

--wygenerowanie listy dziewięciu (lub mniej jesli na brzeach) pozycji do odwiedzenia wokol podanej pozycji (włączenie z tą pozycją)
generateNeighbours :: Board -> Pos -> [Pos]
generateNeighbours board (i,j) = [( i + a, j + b) | a <- [-1,0,1], b <- [-1,0,1], i + a >= 0, j + b >=0, i +a < numColumns board, j+b < numRows board]

-- wygenerowanie listy wszystkich punktów
listOfAllPositions :: Board -> [Pos]
listOfAllPositions board = [(ix,iy) | ix <-[0..(numColumns board - 1)], iy <- [0..(numRows board - 1)]]

-- wyłuskanie wartości z Maybe Int
fromJust :: Maybe Int -> Int
fromJust (Just a) = a

countPos :: [Pos] -> Board -> (Int,Int,Int)
countPos [] _ = (0,0,0)
countPos (x:xs) board = let state = fst (getSquare board x )
    in if state == UNSOLVED then addCounts (1,0,0) (countPos xs board)
        else if state == FILLED then addCounts (1,1,0) (countPos xs board)
            else addCounts (1,0,1) (countPos xs board)

addCounts :: (Int, Int, Int) -> (Int,Int,Int) -> (Int,Int,Int)
addCounts (i,j,k) (l,m,n) = (i+l, j+m, k+n)


--sprawdzenie czy pozycje wokół podanej pozycji poprawnie wypełnione
checkIfLegal :: Board -> Pos -> Bool 
checkIfLegal board (ix,iy) = let value = snd (getSquare board (ix,iy) )
    in if  value == Nothing then True 
        else (let
        (allPos, fillPos, unfillPos) = countPos (generateNeighbours board (ix, iy) ) board 
        in fillPos <= fromJust value && unfillPos <= allPos - fromJust value) 
            where 
                countPos :: [Pos] -> Board -> (Int,Int,Int)
                countPos [] _ = (0,0,0)
                countPos (x:xs) board = let state = fst (getSquare board x )
                    in if state == UNSOLVED then addCounts (1,0,0) (countPos xs board)
                        else if state == FILLED then addCounts (1,1,0) (countPos xs board)
                            else addCounts (1,0,1) (countPos xs board)

                addCounts :: (Int, Int, Int) -> (Int,Int,Int) -> (Int,Int,Int)
                addCounts (i,j,k) (l,m,n) = (i+l, j+m, k+n)

-- sprawdzenie czy cała plansza dobrze wypełniona
-- szybsza wersja?
checkIfBoardLegal :: Board -> [Pos]-> Bool
checkIfBoardLegal _ [] = True
checkIfBoardLegal  board (x:xs)  
    | checkIfLegal board x = checkIfBoardLegal  board xs
    | otherwise = False

-- checkIfBoardLegal :: Board -> Bool
-- checkIfBoardLegal board = and [checkIfLegal board x | x <- listOfAllPositions board]

-- sprawdzenie czy zagadka rozwiązana
-- szybsza wersja?
checkIfBoardCompleted :: Board -> [Pos] -> Bool
checkIfBoardCompleted _ [] = True
checkIfBoardCompleted board (x:xs)
    | fst (getSquare board x ) /= UNSOLVED = checkIfBoardCompleted board xs
    | otherwise = False

--checkIfBoardCompleted :: Board -> Bool 
--checkIfBoardCompleted board = all ( all checkIfCompleted ) board
--    where
--        checkIfCompleted :: Square -> Bool
--        checkIfCompleted (UNSOLVED, _ ) = False 
--        checkIfCompleted _ = True 

-- rozwiazanie bactracking
solve :: Board -> Maybe Board
solve solv = let allPos = listOfAllPositions solv
    in solveBacktracking (0,0) solv allPos
    where 
        solveBacktracking :: Pos -> Board -> [Pos] -> Maybe Board
        solveBacktracking (ix,iy) solv allPos
            | not (checkIfBoardLegal solv allPos) = Nothing    --czy macierz poprawna
            | checkIfBoardCompleted solv allPos = Just solv
            | otherwise = let 
                            (ix', iy') = if ix + 1 >= length solv then (0,iy + 1) else (ix + 1,iy)
                            fill = updateSquareState solv (ix,iy) FILLED 
                            notFill = updateSquareState solv (ix,iy) NOTFILLED 
                        in case solveBacktracking (ix', iy') fill allPos of
                            Just s -> Just s
                            Nothing -> solveBacktracking (ix', iy') notFill allPos
