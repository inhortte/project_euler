{-- 
An irrational decimal fraction is created by concatenating the
positive integers:

0.123456789101112131415161718192021...

It can be seen that the 12^(th) digit of the fractional part is 1.

If d_(n) represents the n^(th) digit of the fractional part, find the
value of the following expression.

d_(1) × d_(10) × d_(100) × d_(1000) × d_(10000) × d_(100000) × d_(1000000)
--}

import Data.Char (ord)

irrationalF = concatMap show [1..200000]
whichOnes = [10^n - 1 | n <- [0..6]]

ans = product $ map (\n -> ord (irrationalF !! n) - 48) whichOnes
