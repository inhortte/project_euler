module PrimesEtc
    where

import Data.List
--import Data.Set (Set)
import qualified Data.Set as Set

isDivisibleBy :: Integer -> Integer -> Bool
n `isDivisibleBy` d = n `rem` d == 0

lowestDivisor :: Integer -> Integer
lowestDivisor n = lowestDivisor' primes n
    where lowestDivisor' (p:ps) n | n `isDivisibleBy` p = p
                                  | p^2 > n = n
                                  | otherwise = lowestDivisor' ps n

isPrime :: Integer -> Bool
isPrime n | n < 0 = error "go have an apple and chin up"
          | n < 2 = False
          | otherwise = lowestDivisor n == n

primes :: [Integer]
primes = 2 : filter isPrime [3..]

-- the Sieve Of Eratosthenes. n is the upper bound.
sieve :: Integer -> [Integer]
sieve n | n < 2 = error "huh?"
        | otherwise = sieve' [2..n]
        where sieve' [] = []
              sieve' primes@(p:ps) | p * p > n = primes
              sieve' (p:ps) = p : sieve' (filter (\x -> x `mod` p > 0) ps)

-- divisors :: Integer -> [Integer]
-- divisors n | n < 1 = error "huh?"
--            | otherwise = [d | d <- [1..(n `quot` 2)], n `mod` d == 0] ++ [n]

divisors :: Integer -> [Integer]
divisors  = map product . removeDuplicates . tail . subsequences . factor

factor :: Integer -> [Integer]
factor n | n < 1 = error "huh?"
         | n == 1 = []
         | otherwise = d : factor (n `quot` d)
         where d = lowestDivisor n

maxDivisors :: [Integer] -> (Integer, Integer)
maxDivisors [] = error "huh?"
maxDivisors ns = maxDivisors' (1, 1) ns
    where maxDivisors' (highest, amount) [] = (highest, amount)
          maxDivisors' (highest, amount) (n:ns) =
              maxDivisors' (if ds > amount
                            then (n, ds)
                            else (highest, amount)) ns
                  where ds = (toInteger . length . removeDuplicates . tail . subsequences . factor) n

