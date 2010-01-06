import Primes
import StringInt
import Data.List (permutations)

sequences = [(x,y) | y <- [11..3333], x <- [1009,1011..7000], arePermutations [x, x + y, x + y + y]]

arePermutations :: [Integer] -> Bool
arePermutations (x:xs) = arePermutations' (permutations $ show x) (map show xs)
    where arePermutations' perms xs = all (`elem` perms) xs

ans = filter (\s -> all isPrime [fst s,fst s + snd s,fst s + snd s + snd s]) sequences

-- let's try it another way.

-- here's the beginning:
filter isPrime $ map asInteger $ nub $ permutations $ show 2969
