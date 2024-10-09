import GHC.Base (VecElem(Int16ElemRep))
{-
Name    : Daniel Nguyen
UIN     : 831003833
email   : danielnguyen@tamu.edu
-}


-- Q1 -- Fixing quicksort
                        -- Add type anotation
quicksort :: [Int] -> [Int]
quicksort  []     = []
quicksort  (x:xs) = quicksort  zs ++ [x] ++ quicksort ys
           where
             ys = [a | a <- xs, a < x]
             zs = [b | b <- xs, b >= x]



-- Q2 -- randomCalculation. Revise number systems if needed
randomCalc :: Int -> Int
randomCalc n = (n * 10 + n) + (n * 100 + (n*10 + n)) + n


-- Q3 -- nth Lucus number
lucas :: Int -> Int
lucas 0 = 2 -- base case
lucas 1 = 1 -- base case
lucas l = lucas (l-1) + lucas (l-2)


-- Q4 -- average of a List
average :: [Int] -> Double
average x = fromIntegral (sum x) / fromIntegral (length x)


-- Q5 -- Grade calculation type
data Scores = Scores Double Double Double Double
  deriving (Show)

classAverage :: Scores -> Double
classAverage (Scores classAvg _ _ _) = classAvg

hwAverage :: Scores -> Double
hwAverage (Scores _ hwAvg _ _) = hwAvg

midterm :: Scores -> Double
midterm (Scores _ _ midtermScore _) = midtermScore

final :: Scores -> Double
final (Scores _ _ _ finalScore) = finalScore


-- Q6 -- total score calculation for grade
totalPoints :: Scores -> Double
totalPoints (Scores classAvg hwAvg midtermScore finalScore) = classAvg * 0.10 + hwAvg * 0.45 + midtermScore * 0.225 + finalScore * 0.225

-- Q7 -- Polynomial calculator 
polyCalcFactory :: [Int] -> Int -> Int
polyCalcFactory coeffs x = evalPolyHelper coeffs x (length coeffs - 1)

-- Recursive helper function, takes in a list of coefficients, x, and the current exponent
evalPolyHelper :: [Int] -> Int -> Int -> Int
evalPolyHelper [] _ _ = 0
evalPolyHelper (c:cs) x exp = c * x^exp + evalPolyHelper cs x (exp - 1)

polyCalcFactory2 :: [Int] -> Int -> Int
polyCalcFactory2 [] _ = 0
polyCalcFactory2 (x:xs) n = sum (x:xs)
  where
    sum :: [Int] -> Int
    sum [] = 0
    sum (x:xs) = x * n ^ count xs + sum xs -- for each coefficient x, multiply x * n ^ count xs, where count xs is the length of the rest of the list
    -- for example, 1x^2 , count is 2 because there are 2 elements in the list [1], n is the input, and x is the list of coefficients.
    count :: [Int] -> Int
    count [] = 0
    count (y:ys) = length (y:ys)

