{-# LANGUAGE BlockArguments #-}
import Prelude
import System.IO (readFile)


a::IO (Char, Char)
a = do  x<-getChar 
        getChar
        y<-getChar
        return (x, y)

readPuzzle :: String -> IO [String]
readPuzzle filename = do
    contents <- readFile filename
    let puzzle = read contents :: [String]
    return puzzle

main = do   puzzle <- readPuzzle "puzzle.txt"
            putStrLn (show $ puzzle)


-- implementacja macierzy
type Matrix = [[Int]]
type Pos = (Int,Int)

-- liczba wierszy macierzy
numRows :: Matrix -> Int
numRows = length 

--liczba kolumn macierzy --funtion composition .
numColumns :: Matrix -> Int
numColumns = length . head 

--stworzenie macierzy o podanej wielkosci wypełnionej wartością c , b - liczba wierszy, a - liczba kolumn
createMatrix :: Int-> Int -> a -> [[a]]
createMatrix a b c = replicate b (replicate a c)

--wydrukowanie wartosci macierzy m na pozycji (a, b) !! - indeksowanie maierzy
elemMatrix :: Matrix -> Pos -> Int
elemMatrix m (a, b) = m !! a !! b


--zmienianie wartosci glowy listy
modifyHead :: (a->a) -> [a] -> [a]
modifyHead _ [] = []
modifyHead mdf (x:xs) = mdf x : xs

--zmienienie wartosci listy w dowolnym miejscu
modifyAt :: Int -> (a->a) -> [a] -> [a]
modifyAt i mdf xs = let (prev, remain) = splitAt i xs
                    in prev ++ modifyHead mdf remain

--zmienianie wartości macierzy iy - wiersz, ix - kolumna --wywolanie updateElemMatrix (1,1) (\x -> 4) macierz
updateElemMatrix :: Pos -> (a->a) -> [[a]] -> [[a]]
updateElemMatrix (iy, ix) mdf = modifyAt iy (\row -> modifyAt ix mdf row)

--usuniecie niepotrzebnych znakow
removePunc :: String -> String
removePunc xs = [ x | x <- xs, not (x `elem` "[]\"") ]

removePuncMatrix :: [String] -> [String]
removePuncMatrix p = map removePunc p

--konwersja do Inta
extractValues :: Char -> Int 
extractValues a     | a == '.' = 10
                    | otherwise = read [a]::Int 

extractValuesList :: String -> [Int]
extractValuesList s = map extractValues s


toIntMatrix :: [String] -> [[Int]]
toIntMatrix m = map extractValuesList m




