module Main where

import Primes
import Data.List (nub, maximumBy)
import Data.Ord (comparing)

totient :: Integer -> Float
totient n = (product . map ((\n -> 1.0 - 1.0 / n) . fromInteger) . nub $ factor n) * (fromInteger n)

getMax :: (Num a, Fractional b, Ord b) => [(a,b)] -> (a,b)
getMax nums = getMax' (0,0.0) nums
    where getMax' m (n:ns) | snd n > snd m = getMax' n ns
                           | otherwise         = getMax' m ns
          getMax' m [] = m

nOverPhis = [(n, fromInteger n / totient n) | n <- [1..999999]]
ans = getMax nOverPhis

main = print ans
