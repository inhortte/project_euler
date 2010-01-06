import Data.List
import Data.Char

-- problem 1: sum of all multiples of 3 or 5 below 1000.
-- Is y a multiple of x?
multipleOf :: Int -> Int -> Bool
multipleOf 0 0 = True
multipleOf 0 _ = False
multipleOf _ 0 = False
multipleOf x y = rem y x == 0

sumMuls3Or5 = sum . filter (\x -> multipleOf 3 x || multipleOf 5 x)

-- problem 2: sum of all even-valued terms in a fibonacci sequence < 4*10^6.
fib :: Int -> [Int]
fib limit = reverse (fib' [1,0])
    where fib' ns@(n_1:n_2:_) = if n < limit
                                then fib' (n:ns)
                                else ns
              where n = n_1 + n_2

sumEvenFibsUpTo = sum . filter even . fib

-- problem 4: largest palindromic number made from the product of two
-- three digit numbers.
l_p_n = maximum [p | q <- [100..999], r <- [q..999], let p = q*r, let s = show p, s == reverse s]
          
-- problem 5: what is the smallest number which is evenly divisible by
-- 1 .. 20?
isDivisibleBy :: Int -> Int -> Bool
n `isDivisibleBy` d = n `rem` d == 0

--lowestDivisor :: Int -> Int
--lowestDivisor n = lowestDivisor' 2
--    where lowestDivisor' d | d * d > n = n
--                           | n `isDivisibleBy` d = d
--                           | otherwise = lowestDivisor' (if odd d
--                                                         then d + 2
--                                                         else 3)

-- this uses isPrime and primes from below.
lowestDivisor :: Int -> Int
lowestDivisor n = lowestDivisor' primes n
    where lowestDivisor' (p:ps) n | n `isDivisibleBy` p = p
                                  | p^2 > n = n
                                  | otherwise = lowestDivisor' ps n

factor :: Int -> [Int]
factor n | n < 1 = error "huh?"
         | n == 1 = []
         | otherwise = d : factor (n `quot` d)
         where d = lowestDivisor n

myLcm :: Int -> Int -> Int
myLcm x y = foldr (*) 1 ((xs \\ ys) ++ ys)
    where xs = factor x
          ys = factor y

lowestCommonMultiple :: [Int] -> Int
lowestCommonMultiple = foldr myLcm 1

-- sum of squares & square of sum.
sumOfSquares :: [Int] -> Int
sumOfSquares = sum . map (^2)

squareOfSum :: [Int] -> Int
squareOfSum = (^2) . sum

-- find the 10001st prime.
isPrime :: Int -> Bool
isPrime n | n < 1 = error "go have an apple and chin up"
          | n == 1 = False
          | otherwise = lowestDivisor n == n

primes :: [Int]
primes = 2 : filter isPrime [3..]

-- problem 8: greatest product of 5 consecutive digits in a number.
productOfDigits :: String -> Int
productOfDigits = foldr ((*) . digitToInt) 1

prodsConsDigits :: Int -> String -> [Int]
prodsConsDigits _ "" = []
prodsConsDigits n digits@(_:ds) | length digits < n = []
                                | otherwise =
                                    productOfDigits (take n digits) :
                                                    prodsConsDigits n ds

-- make the above compare one at a time as it is computed so it does not
-- have to build a big thurking list.

prodsConsDigits2 :: Int -> String -> Int
prodsConsDigits2 _ "" = 0
prodsConsDigits2 n digits = prodsConsDigits2' 0 n digits
    where prodsConsDigits2' highest n digits@(_:ds)
              | length digits < n = highest
              | otherwise =
                  prodsConsDigits2' (max highest (productOfDigits (take n digits))) n ds

-- problem 9: find a * b * c where a^2 + b^2 = c^2 & a + b + c = 1000
problem_9 = a * b * (1000 - a - b)
    where (a, b, csq) = head (filter (\(a, b, csq) -> csq == (1000 - a - b) ^ 2) [(a, b, a ^ 2 + b ^ 2) | a <- [1..999], b <- [a..999]])

-- problem 10: find the sum of all primes < 2 000 000
-- the Sieve Of Eratosthenes. n is the upper bound.
-- Remember that my problem before was using Int instead of Integer.
sieve :: Integer -> [Integer]
sieve n | n < 2 = error "huh?"
        | otherwise = sieve' [2..n]
        where sieve' [] = []
              sieve' primes@(p:ps) | p * p > n = primes
              sieve' (p:ps) = p : sieve' (filter (\x -> x `mod` p > 0) ps)

problem_10 = sum (sieve 2000000)

