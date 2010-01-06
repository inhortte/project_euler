-- Using names.txt (right click and 'Save Link/Target As...'), a 46K 
-- text file containing over five-thousand first names, begin by 
-- sorting it into alphabetical order. Then working out the alphabetical 
-- value for each name, multiply this value by its alphabetical position
-- in the list to obtain a name score.

-- For example, when the list is sorted into alphabetical order,
-- COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th
-- name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?

module Main
    where

import StringInt
import Data.List (sort)
import Data.Maybe (fromJust)

sumWord :: String -> Integer
sumWord = foldr charToInteger 0
    where charMap = zip ['A'..'Z'] [1..26]
          charToInteger c cs = (fromJust $ lookup c charMap) + cs

wordsToList :: String -> [String]
wordsToList wl = map (init . tail) $ splitWith (==',') wl

doIt :: [String] -> Integer
doIt names = foldr addUmUp 0 $ zip [1..] names
    where addUmUp n ns = (fst n) * (sumWord $ snd n) + ns

main = do
  names <- readFile "names.txt"
  putStrLn $ show $ doIt $ sort $ wordsToList names
