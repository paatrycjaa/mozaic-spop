import Data.List (transpose)
import Data.List (intercalate)


showMat = unTable " " . equalizeCellLengths . (map . map) show

main = putStrLn $ showMat [[1,23,456],[78,-90,123],[4567,8,9]]



-- | Un-tabs, and intersperses the result with \\n. This converts a 2-dimensional
--   list of strings into a string where ends of a line are \\n, and the
--   individual cells are divided by \\t.
unTable :: String     -- ^ Column spacer
        -> [[String]] -- ^ Table
        -> String
unTable colSpacer = intercalate "\n" . map (intercalate colSpacer)



-- | Finds the length of the longest line of a table
maxLineLength :: [[a]] -> Int
maxLineLength = maximum . map length



-- | Calculates the length of the longest entry of each column.
maxCellLengthsPerColumn :: [[[a]]] -> [Int]
maxCellLengthsPerColumn = map maxLineLength . transpose



-- | Reformats the table.
equalizeCellLengths :: [[String]] -> [[String]]
equalizeCellLengths table = map (equalize maxLengths) table
	where
		maxLengths = maxCellLengthsPerColumn table

		-- equalize takes a list of lengths and a 2-dimensional table, and
		-- pads the columns so all entries have the same length.
		equalize = flip $ zipWith (padToLengthLeft ' ')



-- | Pads a list to a certain length on the right.
--
--   >>> padToLengthRight '.' "abc" 5
--   "abc.."
padToLengthLeft :: a   -- ^ Padding element
                -> [a] -- ^ List to be padded
                -> Int -- ^ Length to pad to
                -> [a]
padToLengthLeft padding list n = (replicate (n - length list) padding) ++ list


-- ALIGN COLUMNS LEFT:
-- padToLengthRight padding list n = take n (list ++ repeat padding)
