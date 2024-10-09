module Main where
{-
Name    : Daniel Nguyen
UIN     : 831003833
email   : danielnguyen@tamu.edu
-} 


-- Q1 -- CSV file. Grade claculations.

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

    -- only want two decimal places
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


-- Q2 : matrix Multiplication

data Matrix = Matrix [[Int]] deriving (Show)

-- define instance of (*) for matrices. 
-- implement Num typeclass, for my matrix type
instance Num Matrix where
    (*) = multiplyMatrix
    fromInteger n = Matrix [[fromInteger n]]
    negate (Matrix a) = Matrix (map (map negate) a)
    abs (Matrix a) = Matrix (map (map abs) a)
    signum (Matrix a) = Matrix (map (map signum) a)
    (+) = undefined
    (-) = undefined

-- multiply function
multiplyMatrix :: Matrix -> Matrix -> Matrix
multiplyMatrix (Matrix a) (Matrix b) = Matrix [[productMatrix row col | col <- transpose b] | row <- a]

-- calculate the productMatrix of two vectors
productMatrix :: [Int] -> [Int] -> Int
productMatrix [] _ = 0
productMatrix _ [] = 0
productMatrix (x:xs) (y:ys) = x * y + productMatrix xs ys

-- transpose matrix
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)




-- Q3 tree traversals
data Tree a b = Leaf a | Branch b (Tree a b) (Tree a b)

-- This is a generalized version of expression tree from last week.

-- node, left, right
preorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
preorder leafVal branchVal (Leaf a) = [leafVal a]

preorder leafVal branchVal (Branch b left right) =
    branchVal b : preorder leafVal branchVal left ++ preorder leafVal branchVal right

-- left, node, right
inorder :: (a -> c) -> (b -> c) -> Tree a b -> [c] 
inorder leafVal branchVal (Leaf a) = [leafVal a]

inorder leafVal branchVal (Branch b left right) =
    inorder leafVal branchVal left ++ [branchVal b] ++ inorder leafVal branchVal right

-- left, right, node
postorder :: (a -> c) -> (b -> c) -> Tree a b -> [c]
postorder leafVal branchVal (Leaf a) = [leafVal a]

postorder leafVal branchVal (Branch b left right) =
    postorder leafVal branchVal left ++ postorder leafVal branchVal right ++ [branchVal b]

