import Data.Char (digitToInt)

asInt :: String -> Int
asInt s | null s = 0
        | '.' `elem` s = error "Not an integer."
        | head s == '-' = (-1) * foldl intify 0 (tail s)
        | otherwise = foldl intify 0 s
        where intify c c' = 10 * c + digitToInt c'

isDivisibleBy :: Int -> Int -> Bool
n `isDivisibleBy` d = n `rem` d == 0

lowestDivisor :: Int -> Int
lowestDivisor n = lowestDivisor' primes n
    where lowestDivisor' (p:ps) n | n `isDivisibleBy` p = p
                                  | p^2 > n = n
                                  | otherwise = lowestDivisor' ps n

isPrime :: Int -> Bool
isPrime n | n < 1 = error "go have an apple and chin up"
          | n == 1 = False
          | otherwise = lowestDivisor n == n

primes :: [Int]
primes = 2 : filter isPrime [3..]

rotateL :: String -> String
rotateL (s:ss) = ss ++ [s]

isCircularPrime :: Int -> Bool
isCircularPrime n = isCircularPrime' len s
    where s = show n
          len = length s
          isCircularPrime' len s | len == 0 = True
                                 | (isPrime . asInt) s = isCircularPrime' (len-1) (rotateL s)
                                 | otherwise = False

