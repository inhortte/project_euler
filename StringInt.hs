module StringInt
    where

import Data.Char (digitToInt,ord,chr)

asInteger :: String -> Integer
asInteger s | null s = 0
            | '.' `elem` s = error "Not an integer."
            | head s == '-' = (-1) * foldl intify 0 (tail s)
            | otherwise = foldl intify 0 s
            where intify c c' = 10 * c + (toInteger . digitToInt) c'

charSub1 :: Char -> Char
charSub1 = chr . (+(-1)) . ord

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f [] = []
splitWith f l = fst broken : splitWith f (safeTail (snd broken))
    where broken = break f l

