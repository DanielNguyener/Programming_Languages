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
totalPoints (Scores classAvg hwAvg midtermScore finalScore) = classAvg * 0.05 + hwAvg * 0.45 + midtermScore * 0.25 + finalScore * 0.25

-- Q7 -- Polynomial calculator
polyCalcFactory :: [Double] -> Double -> Double
polyCalcFactory [] _ = 0
polyCalcFactory (x:xs) n = x * n ^ count + polyCalcFactory xs n
    where count = length xs

