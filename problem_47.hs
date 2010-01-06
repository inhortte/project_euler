import Primes
import Data.List (nub)

hasDistinctFacts :: Integer -> Integer
hasDistinctFacts = toInteger . length . nub . factor

seqOfThree :: Integer -> [Integer]
seqOfThree n = map hasDistinctFacts $ [n, n+1, n+2, n+3]

seqMap :: [Integer] -> [(Integer, [Integer])]
seqMap = map (\n -> (n, seqOfThree n))

ans = map fst $ filter (\a -> all (==4) $ snd a) $ seqMap [1..1000000]

ansAlt = [x | x <- [1..], all (==4) $ map (toInteger . length . nub . factor) $ [x, x+1, x+2, x+3]] !! 0

