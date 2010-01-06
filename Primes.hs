module Primes
    where

import Data.List

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

factor :: Integer -> [Integer]
factor n | n < 1 = error "huh?"
         | n == 1 = []
         | otherwise = d : factor (n `quot` d)
         where d = lowestDivisor n

-- divisors :: Integer -> [Integer]
-- divisors n = nub $ lesser ++ greater
--    where sqRoot = truncate $ realToFrac $ sqrt $ fromRational $ fromIntegral n
--          lesser = [d | d <- [1..sqRoot], n `rem` d == 0]
--          greater = reverse $ tail $ map (n `div`) lesser

-- Possibly a faster way!
divs :: [Integer] -> Integer -> Integer -> [Integer]
divs sofar d n | d * d > n      = sofar
               | d * d == n     = d : sofar
               | n `rem` d == 0 = divs (d : (n `div` d) : sofar) (d + 1) n
               | otherwise      = divs sofar (d + 1) n

divisors :: Integer -> [Integer]
divisors n = 1 : divs [] 2 n

sumDivisors = foldr (+) 0 . divisors

