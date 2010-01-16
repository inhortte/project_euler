{--
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
--}

module Main where

import System.Environment

coins = ukCoins

ukCoins = [1,2,5,10,20,50,100,200]
usCoins = [1,5,10,25]

countCoins :: Int -> Int -> Int
countCoins amount coin | amount == 0             = 1
                       | amount < 0              = 0
                       | coin < 0 && amount > 0  = 0
                       | otherwise               = (countCoins amount (coin - 1)) + (countCoins (amount - (coins !! coin)) coin)

recursiveAns = countCoins 200 $ (length coins) - 1

main = do
  args <- getArgs
  print $ countCoins ((read $ head args) :: Int) $ (length coins) - 1


