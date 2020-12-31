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
--            mozaic = toIntMatrix (removePuncMatrix puzzle)
--            solution = createMatrix (numColumns mozaic) (numRows mozaic) 2
--            filled_solution = fillAroundWhenVal mozaic solution (listOfCorners mozaic) 4



-- implementacja macierzy
type Matrix = [[Int]]
type Pos = (Int,Int) -- iy/ix

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
elemMatrix m (a, b) = m !! b !! a


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

--sprawdzenie czy punkt istnieje
ifExist :: Pos -> Matrix -> Bool 
ifExist (iy,ix) m   | (iy < 0) || (ix < 0)  || ix > numColumns m || iy > numRows m = False 
                    | otherwise = True

--sprawdzenie,czy pozycja wypelniona
ifFilled :: Pos -> Matrix -> Bool 
ifFilled (iy,ix) m  | elemMatrix m (iy,ix) /= 2 = True
                    | otherwise = False 

--wygenerowanie listy rogów
listOfCorners :: Matrix -> [Pos]
listOfCorners m = [(iy,ix) | iy <- [0, numRows m - 1], ix <- [0, numColumns m - 1], ifExist (iy,ix) m]

--wygenerowanie listy poziomych brzegow
listOfHorizontalBorders :: Matrix -> [Pos]
listOfHorizontalBorders m = [(iy,ix) | iy <-[0, numRows m - 1], ix <- [1..(numColumns m - 2)], ifExist (iy,ix) m]

--wygenerowanie listy pionowych brzegow
listOfVerticalBorders :: Matrix -> [Pos]
listOfVerticalBorders m = [(iy,ix) | ix <-[0, numColumns m - 1], iy <- [1..(numRows m - 2)], ifExist (iy,ix) m]

--wygenerowanie listy elementów poza brzegami (9, ale i 8,7 nie mogą być na brzegu)
listOfInsideElements :: Matrix -> [Pos]
listOfInsideElements m = [(iy,ix) | ix <-[1..(numColumns m - 2)], iy <- [1..(numRows m - 2)], ifExist (iy,ix) m]

--wygenerowanie listy wszytskich punktow
listOfAllPositions :: Matrix -> [Pos]
listOfAllPositions m = [(iy,ix) | ix <-[0..(numColumns m - 1)], iy <- [0..(numRows m - 1)], ifExist (iy,ix) m]


--wygenerowanie listy dziewięciu pozycji do odwiedzenia wokol podanej pozycji
generateFillsPos :: Matrix -> Pos -> [Pos]
generateFillsPos m (i,j) = [( i + a, j + b) | a <- [-1,0,1], b <- [-1,0,1], ifExist (i + a,j +b) m]


--zapelnienie wartosci na podstawie podanej listy pozycji
fillAround :: Matrix -> [Pos]-> Int -> Matrix
fillAround solv [] _ = solv
fillAround solv (x:xs) val = fillAround (updateElemMatrix x (\ y -> val) solv) xs --nie dziala dla zmiennej wartosci :(())

--zapelnienie wartosci wokoł pozycji, ktore maja okreslona wartosc
fillAroundWhenVal :: Matrix -> Matrix -> [Pos] -> Int -> Int -> Matrix
fillAroundWhenVal _ solv [] _ _= solv
fillAroundWhenVal m solv (x:xs) val fil=   if elemMatrix m x == val 
                                        then fillAroundWhenVal m (fillAround solv (generateFillsPos m x) fil) xs val fil
                                        else fillAroundWhenVal m solv xs val fil

--zwrocenie liczby wypelnionych pozycji wokoł
numberOfFilledPos :: Matrix -> Pos -> Int
numberOfFilledPos m (iy,ix) = length [( iy + a, ix + b) | a <- [-1,0,1], b <- [-1,0,1], ifExist (iy + a,ix +b) m, ifFilled (iy + a,ix +b) m ]

--zwrocenie liczby niewypelnionych pozycji wokoł
numberNotOfFilledPos :: Matrix -> Pos -> Int
numberNotOfFilledPos m (iy,ix) = length [( iy + a, ix + b) | a <- [-1,0,1], b <- [-1,0,1], ifExist (iy + a,ix +b) m, not (ifFilled (iy + a,ix +b) m )]

--zwrocenie liczby wszystkich pozycji wokol
numberOfAllPos :: Matrix -> Pos -> Int
numberOfAllPos m (iy,ix) = length (generateFillsPos m (iy,ix))

--wypelnienie pozostałych pozycji wokol wartoscia n
fillAroundNotFilled :: Matrix -> Pos-> Int -> Matrix
fillAroundNotFilled m (iy,ix) val = fillAround m  [( iy + a, ix + b) | a <- [-1,0,1], b <- [-1,0,1], ifExist (iy + a,ix +b) m, not (ifFilled (iy + a,ix +b) m )] val

--wypełnienie oczywistych pozycji
fillObviousPos :: Matrix -> Matrix -> [Pos]-> Matrix
fillObviousPos _ solv [] = solv
fillObviousPos m solv (x:xs) =    if ifFilled x solv == True 
                                then fillObviousPos m solv xs
                                else if elemMatrix m x == numberOfFilledPos solv x
                                    then fillObviousPos m (fillAroundNotFilled solv x 0) xs
                                    else if numberNotOfFilledPos solv x == (numberOfAllPos m x) - (elemMatrix m x)
                                        then fillObviousPos m (fillAroundNotFilled solv x 1) xs
                                        else fillObviousPos m solv xs