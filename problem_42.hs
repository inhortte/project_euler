-- The n^(th) term of the sequence of triangle numbers is given by,
-- t_(n) = ½n(n+1); so the first ten triangle numbers are:

-- 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

-- By converting each letter in a word to a number corresponding to 
-- its alphabetical position and adding these values we form a word value. 
-- For example, the word value for SKY is 19 + 11 + 25 = 55 = t_(10). If 
-- the word value is a triangle number then we shall call the word a 
-- triangle word.

-- Using words.txt (right click and 'Save Link/Target As...'), a 16K text 
-- file containing nearly two-thousand common English words, how many are 
-- triangle words?

import Primes
import StringInt
import Data.List
import Data.Maybe (fromJust)
import System.IO

alphaMap :: [(Char,Integer)]
alphaMap = zip ['A'..'Z'] [1..26]

sumWord :: String -> Integer
sumWord = foldr charToInteger 0
    where charToInteger c cs = (fromJust $ lookup c alphaMap) + cs

isTriangleNumber :: Integer -> Bool
isTriangleNumber n = (==) n $ last $ takeWhile (<=n) triangleNumbers

wordsToList :: String -> [String]
wordsToList wl = map (init . tail) $ splitWith (==',') wl

main = do
  wordsContent <- readFile "words"
  let wordList = filter (isTriangleNumber . sumWord) $ wordsToList wordsContent
  mapM putStrLn wordList
  putStrLn $ show $ length wordList

