
vowels :: [Char]
vowels = "aeiouAEIOU"

isVowel :: Char -> Bool
isVowel c = c `elem` vowels

removeVowels :: String -> String
removeVowels []     = []
removeVowels (x:xs) = if isVowel x
                      then removeVowels xs
                      else x : removeVowels xs

                