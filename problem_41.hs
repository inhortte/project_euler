import Primes
import StringInt
import Data.List

-- We shall say that an n-digit number is pandigital if it makes use of
-- all the digits 1 to n exactly once. For example, 2143 is a 4-digit
-- pandigital and is also prime.
-- 3, 5, 6, 8, and 9 digit pandigitals are all divisible by 3, so
-- I'm only checking 4 and 7 digit pandigitals.

-- What is the largest n-digit pandigital prime that exists?

ans = maximum $ filter isPrime $ map asInteger $ concat $ map permutations [y | x <- ['4','7'], y <- [[x,charSub1 x..'1']]]


