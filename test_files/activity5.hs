import System.IO


ioMaxChar :: IO ()
ioMaxChar = do
    x <- getLine
    y <- getLine
    z <- getLine

    let num1 = read x :: Int
    let num2 = read y :: Int
    let num3 = read z :: Int

    let maximumNum = max num1 (max num2 num3)
    print maximumNum
