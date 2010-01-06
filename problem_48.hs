ans = (\x -> x `mod` 10^10) $ sum $ [x ^ x | x <- [1..1000]]
