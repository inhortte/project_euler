module Main where

import Array
import Primes

abundant :: Integer -> Bool
abundant x = sumDivisors x > x

n :: Integer
n = 28124

abundants_arr :: Array Integer Bool
abundants_arr = listArray (1,n) $ map abundant [1..n]

abundants :: [Integer]
abundants = filter (abundants_arr !) [1..n]

rests :: Integer -> [Integer]
rests x = map (x-) $ takeWhile (<= x `div` 2) abundants

isSum :: Integer -> Bool
isSum = any (abundants_arr !) . rests

main = putStrLn $ show $ foldl1 (+) $ filter (not . isSum) [1..n] 
