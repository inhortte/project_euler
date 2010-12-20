-- Define f(n) as the sum of the factorials of the digits of n.
-- For example, f(342) = 3! + 4! + 2! = 32.
-- Define sf(n) as the sum of the digits of f(n). So sf(342) = 3 + 2 = 5.
-- Define g(i) to be the smallest positive integer n such that
-- sf(n) = i.
-- Though sf(342) is 5, sf(25) is also 5, and it can be
-- verified that g(5) is 25.
-- Define sg(i) as the sum of the digits of g(i). So sg(5) = 2 + 5 = 7.
-- Further, it can be verified that g(20) is 267 and
-- sg(i) for 1 <= i <= 20 is 156.
-- What is  sg(i) for 1 <= i <= 150?

-- This does not finish! I need a better way to compute g.

module Main
    where

import Data.Char
import Data.Maybe (fromJust)

factorial :: Integer-> Integer
factorial n | n < 0 = factorial (-n)
            | n < 2 = 1
            | otherwise = n * factorial (n-1)

intToDigitArray :: Integer-> [Integer]
intToDigitArray = map (\x -> toInteger $ ord x - 48) . show

sumFactorialOfDigitArray :: [Integer] -> Integer
sumFactorialOfDigitArray = sum . map factorial

-- f(n)
f :: Integer-> Integer
f = sumFactorialOfDigitArray . intToDigitArray

-- sf(n)
sf :: Integer-> Integer
sf = sum . intToDigitArray . f

g :: Integer-> Integer
g 1 = 1
g i = (+1) . snd . last . takeWhile (\p -> fst p /= i) . map (\n -> (sf n, n)) $ [1..]

sg :: Integer-> Integer
sg = sum . intToDigitArray . g

ans = sum . map sg $ [1..150]

main = putStrLn $ show $ ans
