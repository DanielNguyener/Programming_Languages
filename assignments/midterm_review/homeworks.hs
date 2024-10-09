import Data.Char (digitToInt, toUpper)


-- Q1 -- Fixing quicksort
-- Add type anotation so that it only works for Int
-- change so that it sorts in descending order.

quicksort:: [Int] -> [Int]
quicksort  []     = []
quicksort  (x:xs) = quicksort  ys ++ [x] ++ quicksort zs
           where
             ys = [a | a <- xs, a >= x]
             zs = [b | b <- xs, b < x]



-- Q2 -- randomCalculation. Revise number systems if needed
randomCalc :: Int -> Int
randomCalc n =
    if n < 1 || n > 9
        then error "Input must be between 1 and 9"
    else
        (n * 10 + n) + (n * 100 + (n * 10 + n)) + n


-- Q3 -- nth Lucus number
lucas :: Int -> Int
lucas x
  | x == 0 = 2 -- base
  | x == 1 = 1 -- base
  | otherwise = lucas (x-1) + lucas (x-2) -- recursion


-- Q4 -- average of a List
average :: [Int] -> Double
average x = fromIntegral (sum x) / fromIntegral (length x) -- have to convert both to double, so that average can return double


-- Q5 -- Grade calculation type
data Scores = Scores {
    classAvg :: Double,
    hwAvg :: Double,
    midtermScore :: Double,
    finalScore :: Double
}


-- Q6 -- total score calculation for grade
totalPoints :: Scores -> Double
totalPoints x = classAvg x * 0.10 + hwAvg x * 0.45 + midtermScore x * 0.225 + finalScore x * 0.225


-- Q7 -- Polynomial calculator
polyCalcFactory :: [Int] -> Int -> Int
polyCalcFactory [] _ = 0
polyCalcFactory (x:xs) n = x * n ^ count + polyCalcFactory xs n
    where count = length xs


-- Q1 -- GCD
gcd' :: Int -> Int -> Int
gcd' x y
  | x < 0 || y < 0 = error "Input must be positive"
  | x == y = x
  | x > y = gcd' (x - y) y
  | otherwise = gcd' x (y - x)


-- Q2 -- unwrapList . 
unwrapList :: [Maybe Int] -> [Int]
unwrapList (x:xs) = [a | Just a <- x:xs] -- list of a, where elements are just a, from x:xs.


-- Q3 -- drop nth 
dropNth :: Int -> [Int] -> [Int]
dropNth x ys = dropNth' x ys 1
    where
        dropNth' _ [] _ = [] -- base case, where the list is empty
        dropNth' x (y:ys) count -- recursion
            | count `mod` x == 0 = dropNth' x ys (count + 1) -- if count is divisible by x, skip the element, increment count.
            | otherwise = y : dropNth' x ys (count + 1) -- otherwise, add the element to the list (y : ys)


-- Q4 -- asInt. Initial code copy given
-- implement using fold, and allow use of negative numbers
asInt :: String -> Int
asInt xs
    | head xs == '-' = - (loop (tail xs))
    | otherwise = loop xs

loop :: String -> Int
loop = foldl (\acc x -> acc * 10 + digitToInt x) 0 -- 0 is initial accumulator, x is current character, based on how many characters we times 10.

-- Q5 -- mergesort
-- Halve function
halve :: [a] -> ([a], [a])
halve xs = (take mid xs, drop mid xs) -- split the list in half
  where
    mid = length xs `div` 2 + length xs `mod` 2 -- find the middle index, for odd xs the first half is 1 element bigger than the second half.
    -- div discards fractionals, so 3/2 is 1. 

-- Merge function
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

-- Mergesort function
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort left) (mergesort right)
  where
    (left, right) = halve xs

-- Q6 -- capitalize, only use higher order functions (map)
capitalize :: [String] -> [String]
capitalize = map (map toUpper)