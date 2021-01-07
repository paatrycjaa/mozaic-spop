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
--            filled_solution = fillAroundWhenVal mozaic solution (listOfCorners mozaic) 4 1
--            filled_2 = fillZeros mozaic solution
--            obvious = fillObviousPos mozaic filled_2 (listOfAllPositions filled_2)



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
removePunc xs = [ x | x <- xs,  x `notElem` "[]\"" ]

removePuncMatrix :: [String] -> [String]
removePuncMatrix  = map removePunc 

--konwersja do Inta
extractValues :: Char -> Int 
extractValues a     | a == '.' = 10
                    | otherwise = read [a]::Int 

extractValuesList :: String -> [Int]
extractValuesList  = map extractValues 


toIntMatrix :: [String] -> [[Int]]
toIntMatrix  = map extractValuesList 

--sprawdzenie czy punkt istnieje
ifExist :: Pos -> Matrix -> Bool 
ifExist (iy,ix) m   | (iy < 0) || (ix < 0)  || ix >= numColumns m || iy >= numRows m = False 
                    | otherwise = True

--sprawdzenie,czy pozycja wypelniona
ifFilled :: Pos -> Matrix -> Bool 
ifFilled (iy,ix) m  | elemMatrix m (iy,ix) /= 2 = True
                    | otherwise = False 

--sprawdzenie, czy pozycja wypelniona konkretna wartoscią?
ifFilledWithValue :: Pos -> Int -> Matrix -> Bool 
ifFilledWithValue (iy,ix) val m     | elemMatrix m (iy,ix) == val = True
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
fillAround solv (x:xs) val = fillAround (updateElemMatrix x (const val) solv) xs val--nie dziala dla zmiennej wartosci :(())

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

--zwrocenie pozycji wokol o zadane wartosci
numberOfFilledWithValue :: Matrix -> Pos -> Int -> Int
numberOfFilledWithValue m (iy,ix) val = length [( iy + a, ix + b) | a <- [-1,0,1], b <- [-1,0,1], ifExist (iy + a,ix +b) m, ifFilledWithValue  (iy + a,ix +b) val m ]

--wypelnienie pozostałych pozycji wokol wartoscia n
fillAroundNotFilled :: Matrix -> Pos-> Int -> Matrix
fillAroundNotFilled m (iy,ix)  = fillAround m  [( iy + a, ix + b) | a <- [-1,0,1], b <- [-1,0,1], ifExist (iy + a,ix +b) m, not (ifFilled (iy + a,ix +b) m )] 

--wypełnienie oczywistych pozycji
fillObviousPos :: Matrix -> Matrix -> [Pos]-> Matrix
fillObviousPos _ solv [] = solv
fillObviousPos m solv (x : xs)
  | elemMatrix m x == numberOfFilledWithValue solv x 1 = fillObviousPos m (fillAroundNotFilled solv x 0) xs
  | numberNotOfFilledPos solv x == elemMatrix m x - numberOfFilledWithValue solv x 1  = fillObviousPos m (fillAroundNotFilled solv x 1) xs
  | otherwise = fillObviousPos m solv xs


--    Assuming you only want to apply function f to elements for which function p returns true, you can do this:
--    map (\x -> if p x then f x else x) xs


-- znajdz na liscie elementy o określonej wartości, zwróć ich listę
findElementswithval :: Matrix -> [Pos] -> Int -> [Pos]
findElementswithval m list val = filter (\x -> elemMatrix m x==val) list 

-- wypelnij wszystkie pozycje wokół zer jedynkami 
fillZeros :: Matrix -> Matrix -> Matrix 
fillZeros m solv = fillAroundWhenVal m solv (listOfAllPositions m) 0 0  


--znajdz pary dwójek w rogach - przypadek oczywisty

--findTwopairs :: Matrix -> [Pos]
--findTwopairs m = map (\x -> generateFillsPos m x) (findElementswithval m listOfCorners 2)

--lista dwójek: findElementswithval m listOfCorners 2  


--pairsOfTwo :: Matrix ->Matrix -> Pos -> Matrix
--pairsOfTwo m solv pos = 
 --   if (elemMatrix m pos ==2) && (length(filter ==2) [( fst pos + a, fst pos + b) | a <- [-1,0,1], b <- [-1,0,1], ifExist (fst pos + a,fst pos +b) m])==2)
 --   then fillAroundWhenVal  m solv [pos] 2 2
  --  else solv    


translateMatrixElements :: Matrix -> Pos -> Char 
translateMatrixElements m pos = if elemMatrix m pos == 0 then '_'
    else 'X'


translateMatrix :: Matrix -> [Char]
translateMatrix m = map (\x -> translateMatrixElements m x) (listOfAllPositions m)

--sprawdzenie czy cała macierz poprawnie wypelniona
checkIfMatrixLegal :: Matrix -> Matrix -> [Pos] -> Bool 
checkIfMatrixLegal _ _ [] = True
checkIfMatrixLegal m solv (x:xs)  
    | checkIfLegal m solv x = checkIfMatrixLegal m solv xs
    | otherwise = False
        where
        --sprawdzenie czy otoczenie wokoł pozycji dobrze wypelnione - czyli liczba 1 nie jest za duza? czy liczba 0 nie jest za duza?
            checkIfLegal :: Matrix -> Matrix -> Pos -> Bool
            checkIfLegal m solv (iy,ix)     | elemMatrix m (iy,ix) == 10 = True
                                            | numberOfFilledWithValue solv (iy,ix) 1 <= elemMatrix m (iy,ix) && numberOfFilledWithValue solv (iy, ix) 0 <= numberOfAllPos solv (iy,ix) - elemMatrix m (iy,ix) = True
                                            | otherwise = False

--sprawdzenie czy macierz wypelniona
checkIfCompleted :: Matrix -> [Pos] -> Bool
checkIfCompleted _ [] = True
checkIfCompleted solv (x:xs)
    | elemMatrix solv x /= 2 = checkIfCompleted solv xs
    | otherwise = False


--rozwiazanie backtracking
solve :: Matrix -> Matrix -> Maybe Matrix
solve m solv = solveBacktracking (0,0) m solv
    where 
        solveBacktracking :: Pos -> Matrix -> Matrix -> Maybe Matrix
        solveBacktracking (iy,ix) m solv 
            | not (checkIfMatrixLegal m solv (listOfAllPositions solv) ) = Nothing    --czy macierz poprawna
            | checkIfCompleted solv (listOfAllPositions solv) = Just solv
            | ifFilled (iy,ix) solv = if ix + 1 >= length solv 
                                        then solveBacktracking (iy + 1, 0) m solv 
                                        else solveBacktracking (iy, ix + 1) m solv
            | otherwise = let 
                            (iy', ix') = if ix + 1 >= length solv then (iy + 1, 0) else (iy, ix + 1)
                            fill = updateElemMatrix (iy,ix) (const 1) solv
                            notFill = updateElemMatrix (iy,ix) (const 0) solv
                        in case solveBacktracking (iy', ix') m fill of
                            Just s -> Just s
                            Nothing -> solveBacktracking (iy', ix') m notFill

                
                    

