

-- Q1

{-
filter is a higher order function
filter :: (a -> Bool) -> [a] -> [a]
returns a list of bools that satisfy the condition

use in conjunction with lambda \ to filter out even numbers
\x -> expression
\x -> x + 1 (add 1 to x)
\x -> x > 3 (return true if x is greater than 3)
\x -> x `mod` 2 == 0 (return true if x is even)

-}

-- define
-- list of doubles, (includes decimals) -> can either be a string if invalid, or int if valid
-- must filter for even, and integers.

-- 2 == fromIntegral(floor 2) == 2.0

evenlistAverage :: [Double] -> Either String Double
evenlistAverage inputList =
    let evenList = filter (\x -> x == fromIntegral (floor x) && (floor x `mod` 2 == 0)) inputList -- even list is resultant double list of filtered numbers
    in if null evenList
        then Left "No even numbers in list"
        else Right (sum evenList / fromIntegral (length evenList)) -- use fromIntegral to convert length to double
--      else Right (fromIntegral (round (sum evenList / fromIntegral (length evenList) * 100)) / 100)
--      this will give two decimal places for double.


-- Q2
-- write a type defined above in the record syntax
-- write a function findCatIndex that returns the index of the first cat that matches the
-- given name and id. The result is type Maybe. If the cat is not found then return nothing
-- write a function getCatByIndex, that recieves a list of Cats and (Maybe index) and returns a Maybe Cat.

data Cat = Cat String Int deriving (Show, Eq)
catID:: Cat -> Int
catID (Cat _ id) = id
catName:: Cat -> String
catName (Cat name _) = name


{-
| are guards that check conditions
otherwise
-}

-- findCatIndex:: [Cat] -> String -> Int -> Maybe Int
-- findCatIndex cats name id = findCatIndex' cats name id 1
--   where
--     findCatIndex' [] _ _ _ = Nothing -- base case
--     findCatIndex' (c:cs) name id index
--         | catName c == name && catID c == id = Just index
--         | otherwise = findCatIndex' cs name id (index + 1)

{-
if then else
-}

findCatIndex:: [Cat] -> String -> Int -> Maybe Int
findCatIndex cats name id = findCatIndex' cats name id 1
    where
        findCatIndex' (x:xs) name id index =
            if catName x == name && catID x == id
                then Just index
                else findCatIndex' xs name id (index + 1)

getCatByIndex:: [Cat] -> Maybe Int -> Maybe Cat
getCatByIndex [] _ = Nothing
getCatByIndex cats (Just index) = Just (cats !! (index - 1))


