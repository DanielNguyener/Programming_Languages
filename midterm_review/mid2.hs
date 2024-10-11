
-- Q6

data Tree a b c = Tree a [Tree a b c]
    | LeafB b
    | LeafC c

-- write down a tree using the above type with at least 10 nodes

exampleTree = Tree
    [ Tree
        [ LeafB "Leaf 1", LeafC 'A' ]
    , Tree
        [ LeafB "Leaf 2", LeafC 'B' ]
    , Tree
        [ LeafB "Leaf 3", LeafC 'C' ]
    , Tree
        [ LeafB "Leaf 4", LeafC 'D' ]
    , Tree
        [ LeafB "Leaf 5", LeafC 'E' ]
    , Tree
        [ LeafB "Leaf 6", LeafC 'F' ]
    , Tree
        [ LeafB "Leaf 7", LeafC 'G' ]
    , Tree
        [ LeafB "Leaf 8", LeafC 'H' ]
    , Tree
        [ LeafB "Leaf 9", LeafC 'I' ]
    , Tree
        [ LeafB "Leaf 10", LeafC 'J' ]
    ]

-- what considerations must be taken into account when defining the functor instance for a tree like this?
{-
a functor requires defining fmap.
we need a function, a tree, and a new tree.
recall that
fmap :: (a -> b) -> f a -> f b

that means, whatever function we use, it must be able to take a, b, c, as arguments.
therefore, our fmap type signature will be
fmap :: (a -> b) -> Tree a b c -> Tree a b c

next, we need to consider traversing the tree
fmap f (Tree xs) = Tree (map (fmap) xs) -> recursively apply fmap to each element in the list
fmap f (leaf b) = f LeafB 
fmap f (leaf c) = f LeafC
-}
-- Answer with reference to the type of fmap


-- Q7
-- define instance of Show for the tree defined, the tree must be printed as a list.
-- use pre-order traversal for generating the list.

instance (Show a, Show b, Show c) => Show (Tree a b c) where
    show t = "[" ++ customShowList t ++ "]"

customShowList :: (Show a, Show b, Show c) => Tree a b c -> String
customShowList (Tree a subtrees) =
    show a ++ concatMap (("," ++) . customShowList) subtrees
customShowList (LeafB b) = show b
customShowList (LeafC c) = show c


-- Q8

-- Function to read and validate input, then print the square
square :: IO ()
square = do
    putStr "Enter a number between 0 and 200: "
    input <- getLine          -- Get user input as a String
    let number = readMaybe input :: Maybe Int  -- Convert input to Maybe Int
    case number of
        Just n | n >= 0 && n <= 200 -> do
            putStrLn $ "Square is: " ++ show (n * n)  -- Print square of the number
        _ -> do -- If input is not a valid number, meaning Just n is Nothing
            putStrLn "Invalid Number."
            square  -- Call square again for a new input

-- Helper function to safely read an Int from a String
readMaybe :: String -> Maybe Int
readMaybe s =
    case reads s of
        [(n, "")] -> Just n
        _         -> Nothing

-- Main function to run the program
main :: IO ()
main = square


-- Q9
{-

1) What is Monad being used for the do syntax?
    state monad because x is using get

2) what is the type of each of the given functions?
    monadSomething' :: State [a] Int
    monadSomething :: [a] -> (Int, [a])
    runState :: State s a -> s -> (a, s)



-}