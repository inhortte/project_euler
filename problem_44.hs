import Primes
import Data.Set (Set)
import qualified Data.Set as Set

-- Damn, that is big.
pNums :: [Integer]
pNums = [x - y | x <- fTPN,
                 y <- takeWhile (<x) fTPN,
                 isP (x - y),
                 isP (x + y)]
    where fTPN = [(n * (3*n-1)) `div` 2 | n <- [1..5000]]
          isP = (`Set.member` Set.fromList fTPN)
                 
-- dS :: [(Integer,Integer,Integer)] -> [Integer]
-- dS = map (\(x,y,d) -> d)

ans = minimum $ pNums

-- I took this following from the answers page. I say 'huh?' and fixed
-- the above I wrote on my own.

solutions = [a-b | a <- penta,
                   b <- takeWhile (<a) penta,
                   isPenta (a-b),
                   isPenta (b+a) ]
isPenta = (`Set.member` Set.fromList penta)
penta = [(n * (3*n-1)) `div` 2 | n <- [1..5000]]

problem_44 = head solutions

