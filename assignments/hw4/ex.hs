module Main where

-- Split a string into a list using character delim
splitWhen :: Char -> String -> [String]
splitWhen _ [] = []
splitWhen delimiter str =
  case break (== delimiter) str of
    (partBefore, []) -> [partBefore]
    (partBefore, _:remaining) -> partBefore : splitWhen delimiter remaining

-- Calculate total and average scores for each student
computeScores :: [[String]] -> [[String]]
computeScores allRows = headerRow : (map totalAverage (tail allRows) ++ [averageRow])
  where
    headerRow = head allRows ++ ["Total"]

    totalAverage studentRow = studentRow ++ [show total]
      where
        scores = map read (tail studentRow) :: [Int]
        total = sum scores

    averageRow = "Average" : map roundAverage averageScores
      where
        scoresLists = map (map read . tail) (tail allRows) :: [[Int]]
        averageScores = map average (transpose scoresLists)

    average xs = fromIntegral (sum xs) / fromIntegral (length xs)

    roundAverage :: Double -> String
    roundAverage x = show (fromIntegral (round (x * 100)) / 100.0)

    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)

-- Read data from the CSV file and process it
processCSV :: String -> String -> IO ()
processCSV inputCSV outputCSV = do
  contents <- readFile inputCSV
  let allRows = map (splitWhen ',') (lines contents)
      processedRows = computeScores allRows
      outputData = unlines (map (addCommas ",") processedRows)
  writeFile outputCSV outputData

-- Combine strings into one, using delimi
addCommas :: String -> [String] -> String
addCommas _ [] = ""
addCommas _ [x] = x
addCommas delim (x:xs) = x ++ delim ++ addCommas delim xs

-- Main function
main :: IO ()
main = do
  let input = "input.csv"    -- Specify your input filename here
      output = "output.csv"  -- Specify your desired output filename here
  processCSV input output