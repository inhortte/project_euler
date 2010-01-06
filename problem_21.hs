module Main
    where

import Primes

ans = sum [x | x <- [1..10000], let s1 = sum $ divisors x, let s2 = sum $ divisors s1, s2 == x && s1 /= x]

main = putStrLn $ show $ ans
