{--
It is possible to write five as a sum in exactly six different ways:

4 + 1
3 + 2
3 + 1 + 1
2 + 2 + 1
2 + 1 + 1 + 1
1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at
least two positive integers?
--}

module Main where

import Data.Maybe (fromJust)

coins = [1, 3, 5]

thurk s cs = thurk' [(0,0)] 1 cs
    where states i cs | i > s = fromJust $ lookup s st'
                   | otherwise = thurk' (st' ++ [smafco 1000000 

smafco states st i [] = states ++ [(i, st)]
smafco states st i js | remains < i = smafco remains i $ tail js
                      | otherwise   = smafco st i $ tail js
                      where remains = i - head js
