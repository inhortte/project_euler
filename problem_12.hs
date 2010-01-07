-- What is the value of the first triangle number to have over
-- five hundred divisors?

-- NOTE: which is not very fast!
-- euler12  22.65s user 0.22s system 90% cpu 25.336 total

-- NOTE: which' is MUCH better. It uses the algorithm to find the number
-- of divisors a number using prime factors described here:
-- http://mathschallenge.net/index.php?section=faq&ref=number/number_of_divisors
-- euler12  0.69s user 0.02s system 72% cpu 0.983 total

module Main where

import Primes
import Data.List (maximumBy, nub, group)
import Data.Ord (comparing)

triangleNumber :: Integer -> Integer
triangleNumber n = n * (n + 1) `quot` 2

triangleNumbers :: [Integer]
triangleNumbers = map triangleNumber [1..]

which = head $ filter (\(n,m) -> m > 500) $ [(n, length . nub . divisors . triangleNumber $ n) | n <- [10000..20000]]

ans = triangleNumber . fst $ which

which' = head $ filter (\(n,m) -> m > 500) $ [(n, product . map ((+1) . length) . group . factor . triangleNumber $ n) | n <- [10000..20000]]

ans' = triangleNumber . fst $ which'

main = print ans'
