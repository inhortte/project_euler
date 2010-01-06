factorial :: Integer -> Integer
factorial n | n < 0 = factorial (-n)
            | n < 2 = 1
            | otherwise = n * factorial (n-1)

addDigits :: Integer -> Integer
addDigits n | n < 0 = addDigits (-n)
            | n == 0 = 0
            | otherwise = n `mod` 10 + addDigits (n `quot` 10)
