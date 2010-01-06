module Main
    where

import Primes
import Data.List
import System.IO

-- ans = last $ filter isPrime $ takeWhile (<1000000) $ map (\n -> sum $ take n primes) [1..]

-- ans = sort $ filter (\x -> isPrime x && x < 1000000) $ takeWhile (<2000000) $ dropWhile (<900000) $ [sum $ take n $ drop m primes | m <- [0..], n <- [1..600]]

maxprime = 547
leastsum = 930000

-- setinc = [take n primes | n <- [1..5]]
setinc = reverse $ tail $ inits $ take maxprime primes
-- setdec = [take (5-m) $ drop m primes | m <- [0..4]]
setdec = init $ tails $ take maxprime primes

-- Opposite (> is LT) because I want them sorted decreasingly.
lengthSort a b | length a > length b = LT
               | otherwise = GT

whatever :: [[Integer]]
whatever = whatever' setinc setdec
    where whatever' [] ys = []
          whatever' (x:xs) ys = isects ++ whatever' xs ys
              where isects = filter (\isect -> (isPrime $ sum isect) && sum isect > leastsum) $ map (intersect x) ys

ans = (map sum $ sortBy lengthSort whatever) !! 0

main = do putStrLn "Here we go with this hovno..."
          (putStrLn . show) ans
