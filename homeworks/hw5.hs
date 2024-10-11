import Control.Monad.State
import qualified Data.Map as Map
{-
Name    : Daniel Nguyen
UIN     : 831003833
email   : danielnguyen7574@gmail.com
-}

data Clist a = End a
            |  Node a (Clist a)
            deriving Show

-- Q1 implementations go here

-- we implement functor
-- we need to define fmap
instance Functor Clist where
    fmap f (End x) = End (f x)
    fmap f (Node x xs) = Node (f x) (fmap f xs)

-- we implement applicative
-- we need to define pure and <*>
instance Applicative Clist where
    (End f) <*> xs = fmap f xs
    (Node f fs) <*> xs = fmap f xs `append` (fs <*> xs)
      where
        append (End x) ys = Node x ys
        append (Node x xs) ys = Node x (xs `append` ys)
    pure = End

-- we implement monad
-- we need to define return and >>=
instance Monad Clist where
    return = pure

    (End x) >>= f = f x
    (Node x xs) >>= f = f x `append` (xs >>= f)
      where
        append (End x) ys = Node x ys
        append (Node x xs) ys = Node x (xs `append` ys)

-- taken from homework prompt.
squares :: Clist Int -> Clist Int
squares lst = do
    x <- lst
    return (x * x)

{-
use this to test squares
testList = Node 1 (Node 2 (Node 3 (End 4)))
squares testList
-}


-- Q2 a
-- simple fib1 function
fib1 :: Int -> Int
fib1 n
    | n == 0 = 0 -- base
    | n == 1 = 1 -- base
    | otherwise = fib1 (n - 1) + fib1 (n - 2) -- recursive

-- Q2b
-- fib2 function using state monad
fib2 :: Int -> State Int Int
fib2 0 = do -- base case
    -- we use get to get the current state
    state <- get
    -- we use put to update the state
    put (state + 1)
    return 0

fib2 1 = do -- base case
    state <- get
    put (state + 1)
    return 1

fib2 n = do -- recursive case
    state <- get
    put (state + 1)
    a <- fib2 (n - 1)
    b <- fib2 (n - 2)
    return (a + b)


-- Q2c
-- fib3 function using state monad and map to store results of previous maps
fib3 :: Int -> State (Map.Map Int Int) Int
fib3 0 = return 0
fib3 1 = return 1
fib3 n = do -- recursive

    -- initialize the map
    maps <- get 

    case Map.lookup n maps of
        -- if the result is in the map, return the result
        Just result -> return result  
        -- if not in map, calculate result
        Nothing -> do

            a <- fib3 (n - 1)
            b <- fib3 (n - 2)
            let result = a + b

            -- we could use modify
            -- modify (Map.insert n result)

            -- if we want to use put, we have to get current map, insert the result, and put it back
            currentMap <- get
            put (Map.insert n result currentMap)

            return result

