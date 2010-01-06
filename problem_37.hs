module Main
    where

import Data.Char (digitToInt)

-- the Sieve Of Eratosthenes. n is the upper bound.
sieve :: Integer -> [Integer]
sieve n | n < 2 = [2]
        | otherwise = sieve' [2..n]
        where sieve' [] = []
              sieve' primes@(p:ps) | p * p > n = primes
              sieve' (p:ps) = p : sieve' (filter (\x -> x `mod` p > 0) ps)

isDivisibleBy :: Integer -> Integer -> Bool
n `isDivisibleBy` d = n `rem` d == 0

lowestDivisor :: Integer -> Integer
lowestDivisor n = lowestDivisor' primes n
    where lowestDivisor' (p:ps) n | n `isDivisibleBy` p = p
                                  | p^2 > n = n
                                  | otherwise = lowestDivisor' ps n

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
          | otherwise = lowestDivisor n == n

primes :: [Integer]
primes = 2 : filter isPrime [3..]

asInt :: String -> Int
asInt s | null s = 0
        | '.' `elem` s = error "Not an integer."
        | head s == '-' = (-1) * foldl intify 0 (tail s)
        | otherwise = foldl intify 0 s
        where intify c c' = 10 * c + digitToInt c'

{-
truncR :: Integer -> Integer
truncR n = n `div` 10
-}

{-
truncL :: Integer -> Integer
truncL n | n < 10 = 0
         | otherwise = (toInteger . asInt . tail . show) n
-}

len :: Integer -> Integer
len n = toInteger $ length $ show n

truncL :: Integer -> [Integer]
truncL n = map ((mod n) . (10^)) [1.. len n]

truncR :: Integer -> [Integer]
truncR n = map ((n `div`) . (10^)) [0.. len n - 1]

truncPrime :: Integer -> Bool
truncPrime n = tPrimeL n && tPrimeR n
    where tPrimeL n = all isPrime $ truncL n
          tPrimeR n = all isPrime $ truncR n
{-
truncPrime :: Integer -> Bool
truncPrime n = tPrimeL n && tPrimeR n
    where tPrimeL n | n == 0 = True
                    | n `elem` sieve n = tPrimeL $ truncL n
                    | otherwise = False
          tPrimeR n | n == 0 = True
                    | n `elem` sieve n = tPrimeR $ truncR n
                    | otherwise = False
-}

ans = sum $ take 11 [x | x <- [11..], truncPrime x]

-- main = putStrLn $ show $ sum [x | x <- [10..1000000], truncPrime x]
