import Control.Exception (ArithException(DivideByZero))
import Distribution.SPDX (LicenseId(BlueOak_1_0_0))
-- L5
-- Error & Types

{-
can't use try/catch
error needs to be contained in output
can only evaluate return value

data Either a or b =    Left a
                    |   Right b

left associated with errors
right report results.

-}
-- factorial function
f 0 = 1
f n = n * (f (n - 1))


-- 
fact n  |   n < 0 = Left "Not Computed"
        | otherwise = Right (f n)

-- calculate fact n, and fact (n - k)
-- both give result, then do nf / n
-- if not, then it's 0.
perm :: (Ord a, Fractional a) => a -> a -> a
perm n k = case (fact n, fact ( n - k)) of 
                (Right nf, Right nkf) -> nf / nkf
                otherwise -> 0

-- make exception data types
data Exception  =    ValueError 
              |       IndexError 
              |       SomeOtherError 
              |       DivideByZeroError 


divide x y | y == 0 = Left DivideByZeroError
           | otherwise = Right (x / y)

-- case is like a try
test x y = case (divide x y) of 
                Left ValueError -> 0
                Left IndexError -> 1
                Left SomeOtherError -> 2
                Left DivideByZeroError -> 0/0
                Right n -> n


{-
Type Classes

what are the operations that are required by the num type class
:i (info command)

:i Num

it is a constrant type
type Num :: * -> Constraint

class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a

These are all functions that Num class can be used in 
-}


check a b = a == b
-- data Color = Red | Green

{-
Red == Green
error: no instance of Color, equalaity (Eq) function is not defined for the type Color.
-}

data Color = Red | Green | Blue

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    _ == _ = False


-- defining your own class
-- you can either define equals or not equals.
class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isEqual2 x y = not (isNotEqual2 x y )

    isNotEqual2 :: a -> a -> Bool
    isNotEqual2 x y = not (isEqual2 x y)

-- implenting our own class type

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

-- super classes (Ex3.hs)


-- example
class (Show b) => MyNum a b where
    myAdd :: a -> a -> b
    mySub :: a -> a -> b

data MyInt = MyInt Int
    deriving(Eq)
    
instance MyNum MyInt String where
    myAdd (MyInt x) (MyInt y) = show (x + y)
    mySub (MyInt x) (MyInt y) = show (x - y)


m = MyInt 3
n = MyInt 20


class Dummy a where
    iAmAString :: a -> String
    iAmABool :: a -> Bool
    iAmADouble :: a -> Double

instance Dummy Int where
    iAmAString x = "int string"
    iAmABool x = TRUE
    iAmADouble x = 23.0