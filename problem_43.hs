import Primes
import StringInt
import Data.List (permutations)

-- Relavent primes.
relPrimes :: [Integer]
relPrimes = takeWhile (<=17) primes

-- pms: primes, pd: pan-digital.
subStrDiv :: [Integer] -> String -> Bool
subStrDiv pms (pd1:pds) = subStrDiv' pms pds
    where 
      subStrDiv' [] _ = True
      subStrDiv' (pm:pms) pand@(pd:pds) | (==) 0 $ (`rem` pm) $ asInteger $ take 3 pand = subStrDiv' pms pds
                                        | otherwise = False

ans = filter (subStrDiv relPrimes) (permutations ['0'..'9'])
