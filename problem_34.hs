import Data.Char (digitToInt)

factorial :: Integer -> Integer
factorial n | n < 0 = error "huh?"
            | n < 2 = 1
            | otherwise = product [2..n]

factSums :: Integer -> [Integer]
factSums m = [x | x <- [2..m], x == (sum $ map (factorial . toInteger . digitToInt) $ show x)]

