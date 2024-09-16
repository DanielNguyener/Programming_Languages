module Hw2 where
import Data.Char ( digitToInt, toUpper )

{-
Name    : Daniel Nguyen
UIN     : 831003833
email   : danielnguyen@tamu.edu
-}


-- Q1 -- GCD
-- putStrLn usage: ex9.hs (week3/code/code/ex9.hs)
gcd' :: Int -> Int -> IO ()
gcd' x y
    | x < 0 || y < 0    = putStrLn "*** Exception: One of the inputs is negative."
    | x == y            = print x
    | x > y             = gcd' (x-y) y
    | otherwise         = gcd' x (y-x)


-- Q2 -- unwrapList
{-
foldr: processes a list from right to left, and builds up a value
lambda function: takes in an element and an acc (accumulator), return acc

for x in the list:
    if x is Just n, add n to the accumulator
    if x is Nothing, skip it
[] is the initial accumulator value (base case)

case of, ex4.hs. ex5.hs (week 4)
Maybe type using Just & Nothing, ex5.hs (week 3) and ex5.hs (week3/code/code/ex5.hs)
foldr usage: ex10.hs (week3/code/code/ex10.hs)
-}
unwrapList :: [Maybe Int] -> [Int]
unwrapList = foldr addToList [] -- helper function
  where
    addToList (Just n) acc = n : acc
    addToList Nothing acc  = acc

{- a more explicit example using recursion, and no foldr,
unwrapList :: [Maybe Int] -> [Int]
unwrapList [] = [] -- base case
unwrapList (Nothing : xs) = unwrapList xs -- if the head is Nothing, skip it
unwrapList (Just n : xs) = n : unwrapList xs -- if the head is Just n, add n to the list
-}

{- a more abstract, and simpler using higher order function (lambda) and foldr.
unwrapList :: [Maybe Int] -> [Int]
unwrapList = foldr (\x acc -> case x of
                                Just n  -> n : acc
                                Nothing -> ACC) [] while using foldr, but not lambda?
-}


-- Q3 -- drop nth 
dropNth :: Int -> [a] -> [a]
dropNth _ [] = [] -- base case, empty list.

dropNth n xs = dropNthHelper n xs 1
  where
    dropNthHelper _ [] _ = [] -- base case, empty list.
    dropNthHelper n (x:xs) count -- recursive case
      | count `mod` n == 0 = dropNthHelper n xs (count + 1) -- if count is divisible by n, skip the element
      | otherwise = x : dropNthHelper n xs (count + 1) -- otherwise, add the element to the list


-- Q4 -- asInt. Initial code copy given
-- Convert a string to an integer, handle negative numbers
-- loop usage: ex10.hs (week3/code/code/ex10.hs)
asInt :: String -> Int
asInt xs
    | head xs == '-' = - (loop (tail xs))
    | otherwise = loop xs

-- Use foldl to to convert all characters to integer, use import digitToInt
loop :: String -> Int
loop = foldl (\acc x -> acc * 10 + digitToInt x) 0

-- Q5 -- mergesort
{-
where usage: ex3.hs (week 4)

-}
-- Halve function
halve :: [a] -> ([a], [a])
halve xs = (take mid xs, drop mid xs) -- split the list in half
  where
    mid = length xs `div` 2 + length xs `mod` 2 -- find the middle index

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

