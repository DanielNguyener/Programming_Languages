{-
Name    : Daniel Nguyen
UIN     : 831003833
email   : danielnguyen@tamu.edu
-}


-- Q1 -- Histogram
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
data BarChart = BarChart {
    labels  :: [String],
    counts  :: [Integer]
}

bar1 = BarChart ["Cat", "Dog", "Others"] [5, 4, 3]

-- Q1 answer goes here

-- Define an instance of show for this histogram

instance Show BarChart where
    show (BarChart [] []) = "" -- base case
    show (BarChart (l:ls) (c:cs)) = barHelper l c ++ show (BarChart ls cs) -- recursive case
        where
            barHelper l c =
                let spaceCount = 10 - length l
                    spaces = makeSpaces spaceCount
                    stars = makeStars c
                in l ++ spaces ++ stars ++ "\n" -- treat label as the line.

            makeSpaces 0 = "" -- base case
            makeSpaces n = ' ' : makeSpaces (n - 1) -- recursive case
            {-
            we use the : operator (concstruct), so ' ' is thought as the head
            and makeSpaces(n-1) is thought of as the rest of the growing string.
            -}

            makeStars 0 = "" -- base case
            makeStars n = '*' : makeStars (n - 1) -- recursive case

-- Q2 : prefix_calc with exception handling

-- Define your exceptions here
-- Define the prefix_calc function
prefix_calc :: String -> Integer -> Integer -> Either String Integer
prefix_calc "+" x y = Right (x + y)
prefix_calc "-" x y = Right (x - y)
prefix_calc "*" x y = Right (x * y)
prefix_calc "/" x 0 = Left "Exception" -- maybe string
prefix_calc "/" x y = Right (div x y ) -- maybe integer

-- Define the evalq2 function
-- parse String into operator, and operands.
-- after parse, use prefix_calc to calculate the result.
evalq2 :: String -> Integer
evalq2 expr =
    case words expr of -- use words to parse string.
        [op1, op2, op3] ->
            let operator = op1
                operand1 = read op2 :: Integer -- convert word to integer
                operand2 = read op3 :: Integer
            in case prefix_calc operator operand1 operand2 of
                Right result -> result
                Left _ -> error "Division by zero"



-- Q3 simple type classes
-- define typeclass that has speak and name functions
class Animal a where
    speak :: a -> String
    name :: a -> String

-- define Cat, Dog, Mouse types that take one string argument
data Cat = Cat String
data Dog = Dog String
data Mouse = Mouse String

-- define instances of type Cat, that implements Animal typeclass (and it's associated functions)
instance Animal Cat where
    speak _ = "Meow" -- speak function called will produce Meow
    name (Cat a) = a -- name function called will extract string value a, from when cat constructed.

instance Animal Dog where
    speak _ = "Woof"
    name (Dog a) = a

instance Animal Mouse where
    speak _ = "Squeak"
    name (Mouse a) = a

-- accept cat dog or mouse, and evaluates to string
whatDoAnimalsSay :: Animal a => a -> String -- a is defined as Animal, -> string
whatDoAnimalsSay a = "I am " ++ name a ++ " and I " ++ speak a




-- Q4 
data MyInt = MyInt Int

-- Instances go here. Do not use deriving for this question.
-- focus: utilize hierachy
-- we're defining instance for Integral, which is a subclass of Num
{-
:info Integral
type Integral :: * -> Constraint
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
-}

-- we need to define instance of Integral for MyInt
-- that can utilize toInteger and quotRem

-- Integral is a subclass of Real and Enum.
instance Integral MyInt where
    toInteger (MyInt x) = toInteger x
    quotRem (MyInt x) (MyInt y) = (MyInt (quot x y), MyInt (rem x y))

-- Enum is not a sublass of anything else,
instance Enum MyInt where
    fromEnum (MyInt x) = x
    toEnum = MyInt

-- Real is a subclass of Num
instance Real MyInt where
    toRational (MyInt x) = toRational x

-- Num is a subclass of Show and Eq.
instance Num MyInt where
    (+) (MyInt x) (MyInt y) = MyInt (x + y)
    (-) (MyInt x) (MyInt y) = MyInt (x - y)
    (*) (MyInt x) (MyInt y) = MyInt (x * y)
    abs (MyInt x) = MyInt (abs x)
    signum (MyInt x) = MyInt (signum x)
    fromInteger x = MyInt (fromInteger x)

-- we define Ord, which is a subclass of Eq
instance Ord MyInt where
    compare (MyInt x) (MyInt y) = compare x y

-- we define Eq
instance Eq MyInt where
    (MyInt x) == (MyInt y) = x == y

-- we define Show parent-class for Num
instance Show MyInt where
    show (MyInt x) = show x


-- Q5
-- Define a data type Expr that can represent expressions with +, -, *, and /
-- this constructor takes three arguments (operator, left expression, right expression)
-- holds an integer value
data Expr  =  Expr (Expr -> Expr -> Expr) Expr Expr
           |  Val Int

instance Show Expr where
    show (Val x) = show x

plus (Val x) (Val y) = Val (x + y)

minus :: Expr -> Expr -> Expr
minus (Val x) (Val y) = Val (x - y)

mul (Val x) (Val y) = Val (x * y)

divide (Val x) (Val y) = Val (div x y)

eval e1@(Val x) = e1 -- base case
eval (Expr op e1 e2) = op (eval e1) (eval e2) -- recursive case

{-
List function calls for expression 1

eval (Expr divide (Expr minus (Val 200) (Expr plus (Val 6) (Val 10))) (Expr mul (Val 3) (Val 4)))
eval (Expr minus (Val 200) (Expr plus (Val 6) (Val 10)))
eval (Val 200)
eval (Expr plus (Val 6) (Val 10))
eval (Val 6)
eval (Val 10)
plus (Val 6) (Val 10)
eval (Expr minus (Val 200) (Val (6 + 10)))
minus (Val 200) (Val (6 + 10))
eval (Expr mul (Val 3) (Val 4))
eval (Val 3)
eval (Val 4)
mul (Val 3) (Val 4)
eval (Expr divide (Val (200 - (6 + 10))) (Val (3 * 4)))
divide (Val (200 = (6 + 10))) (Val 12)
((200) - (6 + 10)) / (3 * 4)
-}




{-
List function calls for expression 2

eval (Expr plus (Val 2) (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2)))))
eval (Val 2)
eval (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2))))
eval (Val 6)
eval (Expr minus (Val 3) (Expr divide (Val 4) (Val 2)))
eval (Val 3)
eval (Expr divide (Val 4) (Val 2))
eval (Val 4)
eval (Val 2)
divide (Val 4) (Val 2)
minus (Val 3) (Val (4 / 2))
mul (Val 6) (Val (3 - (4 / 2)))
plus (Val 2) (Val (6 * (3 - (4 / 2))))
(2) + (6 * (3 - (4 / 2)))
-}

-- expression 1
a = Expr divide (Expr minus (Val 200) (Expr plus (Val 6) (Val 10))) (Expr mul (Val 3) (Val 4))

-- expression 2
b = Expr plus (Val 2) (Expr mul (Val 6) (Expr minus (Val 3) (Expr divide (Val 4) (Val 2))))


