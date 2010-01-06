sumDigits :: Integer -> Integer
sumDigits n | n < 0 = sumDigits (-n)
            | n == 0 = 0
            | otherwise = n `mod` 10 + sumDigits (n `quot` 10)

splud = sumDigits (2 ^ 1000)

