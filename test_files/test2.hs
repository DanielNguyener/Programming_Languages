module Hw2 where
{-
Name    : Daniel Nguyen
UIN     : 831003833
email   : danielnguyen@tamu.edu
-}


-- Q1 -- GCD
gcd1 :: Int -> Int -> IO ()
gcd1 x y
    | x < 0 || y < 0    = error "One of the inputs is negative."
    | x == y            = print x
    | x > y             = gcd1 (x-y) y
    | otherwise         = gcd1 x (y-x)

unwrapList :: [Maybe Int] -> [Int]
unwrapList [] = [] -- base case
unwrapList (Nothing : xs) = unwrapList xs -- if the head is Nothing, skip it
unwrapList (Just n : xs) = n : unwrapList xs -- if the head is Just n, add n to the list

dropNth :: Int -> [a] -> [a]
dropNth _ [] = []

dropNth n xs = dropNthHelper n xs 1
  where
    dropNthHelper _ [] _ = []
    dropNthHelper n (x:xs) count
      | count `mod` n == 0 = dropNthHelper n xs (count + 1)
      | otherwise = x : dropNthHelper n xs (count + 1)