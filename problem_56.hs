{--
A googol (10^(100)) is a massive number: one followed by one-hundred
zeros; 100^(100) is almost unimaginably large: one followed by
two-hundred zeros. Despite their size, the sum of the digits in each
number is only 1.

Considering natural numbers of the form, a^(b), where a, b < 100, what
is the maximum digital sum?
--}

module Main where

import Data.Char (ord)

powers = [foldr (\a b -> ord a - 48 + b) 0 $ show $ x ^ y | x <- [2..99], y <- [1..99]]

main = print $ maximum powers
