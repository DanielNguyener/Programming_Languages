import Debug.Trace

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


-- record syntax
data Cat = Cat {
    catName:: String,
    catID:: Int
} deriving (Show, Eq)


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
if then else usage
-}

-- either way, we require a helper function that counts the index.
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

-- Q3
-- write a function listInsert that inserts and element into a list sorted in ascending order
-- do not insert a value if it is already in the list.Applicative

listInsert :: (Ord a) => a -> [a] -> [a]
listInsert x ys = listInsert' x ys [] -- helper function
    where
        listInsert' x [] acc = x:acc -- base case, empty list, return x.
        listInsert' x (y:ys) acc -- recursive case
            | x == y = acc ++ y:ys -- if x is equal to y, return the accumulated with the rest of list without adding x.
            | x > y = listInsert' x ys (acc ++ [y]) -- if x is greater, then continue: x, ys, accumulated (other greaters) + y (current)
            | x < y = acc ++ x:y:ys -- if x is less, then return accumulated (less) + x (value insert) + y (greater) + ys (rest)

-- Q4
-- type named random a,b where they are type variables.

-- Random is the name of the data type
-- two type parameters, a and b
-- Random { is the constructor}
-- randomX, randomY, randomZ are the fields of the data type, otherwise the three constructors

-- a can be both a string, and an int. This means it must be polymorphic.
data Random a b = 
    RandomX a b
    | RandomY a
    | RandomZ b

-- explain why this is not a type conflict
{-
because Random is a polymorphic data type, a b are type variables, and can be any type.
when we use RandomX, we can any combination of types.
when we use RandomY, because there is no initial assignment of types to a and b, we can still use any type.
-}

-- monomorphic types
data Point = Point Int Int
p1 = Point 1 2
-- p2 = Point "Hello" 3 this will cause an error, because the type is not the same.

-- partial polymorphism
data Partial a = Partial Int a
w1 = Partial 5 "hello"
-- w2 = Partial "Hi" 3.14 this will cause an error because the first argument must be int

-- new types
newtype Age = Age Int
myAge :: Age
myAge = Age 25

{-
badAge :: Int
badAge = Age 25 this will cause an error because Age is a newtype, and not a type synonym
-}

-- Q5
-- 1)
h x y = x ++ (show y)
-- what is the type of the given function?
-- h :: Show a => [Char] -> a -> [Char]

-- x must be a list
-- show y. Show is a typeclass that converts any type that is an instance of Show into a String
-- therefore h must be [Char] or String

-- 2)

-- g is a bool list
-- g a is a bool list.
-- fold takes a function, an accumulator, and a bool list
-- f should be a function that takes a string and a bool list
-- foldr returns a new string.

{-

3)

Describe why the following code snippet has a type error:

add x y = x + y
xyz (x:xs) = add x (head (filter isDigit xs))

add x and y,
y is head (filter isDigit xs)
filter will return a list of digits from xs.
head will return the first element of the list.

add is only supported for Num
even thought filter isDigit xs will return a list of digits, they are still chars.
you cannot add Char to Num.

you need to convert using digitToInt

-}

-- 4)
{-

differnce between trace and traceShow

trace is a function that takes string as an argument, and expression and it returns the evaluation.
traceShow is a function that takes any type of show type class, and expression, and returns 

trace "Hello" (1 + 2) will return
Hello
3
traceShow (1 + 2) "Hello" will return
"3
Hello"
-}