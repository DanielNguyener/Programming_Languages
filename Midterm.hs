
-- Q1


{-

a)
-- f :: String -> String

-}


-- b)

data Table = Table Int Int [[Double]]
    deriving (Show)

rows :: Table -> Int
rows (Table rowsTable _ _ ) = rowsTable

cols :: Table -> Int
cols (Table _ colsTable _) = colsTable

tableData :: Table -> [[Double]]
tableData (Table _ _ dataTable) = dataTable


{-

c)

alternate recieves two functions and a value
if the first function can return maybe

second function returns value

    show x
    show y
    just z
    just y

alternate :: (Show f1, Show f2) => (f1 -> Maybe a1) -> (f1 -> Maybe f2) -> f1 -> String

-}

-- d)

-- myfilter :: (a -> Bool) -> [a] -> [a]
-- myfilter (x:xs) Bool
--     if x == True
--         then myfilter xs a
--     else myfilter xs

-- Q2
data Tree a = Node a (Tree a) (Tree a)
    | Leaf a
    | Nil
    deriving (Eq, Show)

-- insert value in the binary tree
-- smaller in left subtree
-- larger values in right subtree
-- leave tree unchanged if value already exists.
-- tree is allowed to be unbalanced

insertValue :: (Ord a) => a -> Tree a -> Tree a
insertValue x Empty = Node x Empty Empty -- base case
insertValue x (Node value left right)
  | x < value = Node value (insertValue x left) right -- insert lef subtree
  | x > value = Node value left (insertValue x right) -- insert rght subtree
  | otherwise = Node value left right



-- looks for a value in the binary tree
-- if found in tree, return subtree such that value is root
    -- subtree must be wrapped in maybe type
    -- return nothing if value not found

lookupTree :: (Ord a) => a -> Tree a -> Maybe (Tree a)
lookupTree _ Empty = Nothing -- base case
lookupTree x (Node value left right)
    | x == value = Just (Node value left right)  -- found value
    | x < value  = lookupTree x left 
    | otherwise  = lookupTree x right


-- Q3

listAverage :: [Double] -> Either String Double
listAverage a = listAverage' a []
    where listAverage (x:xs) acc =
        if (x < 0 || x > 100) && x != 999
            then "Invalid Data" --outside range
        else if x == 999
            then sum fromIntegral (acc) `div` fromIntegral(length acc) -- stop scanning
        else
            if acc == [] -- empty, no input
                then "No input"
            else if xs == [] -- if there are no more values to be scanned
                then sum fromIntegral (acc) `div` fromIntegral(length acc)
            else acc ++ x


listAverage :: [Double] -> Either String Double
listAverage a = listAverage' a []
  where
    listAverage' [] acc
      | null acc = Left "No input" -- no valid inputs
      | otherwise = Right (sum acc / fromIntegral (length acc)) -- calculate average

    listAverage' (x:xs) acc
      | x < 0 || x > 100 = Left "Invalid Data" -- outside range
      | x == 999 = listAverage' [] acc -- stop scanning
      | otherwise = listAverage' xs (acc ++ [x]) -- accumulate valid numbers