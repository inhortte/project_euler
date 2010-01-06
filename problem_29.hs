import Data.List (union,nub)

-- low, high.
powList :: Integer -> Integer -> [Integer]
powList low high = powList' low high low []
    where
      powList' low high base terms | base > high = terms
                                   | otherwise = powList' low high (base+1) (union terms (map (base^) [low..high]))

-- or
ans = length (nub [a^b | a <- [2..100], b <- [2..100]])
