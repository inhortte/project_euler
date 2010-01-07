-- What is the first term in the Fibonacci sequence to contain 1000 digits?
-- euler25  0.03s user 0.01s system 93% cpu 0.043 total

module Main where

goldenRatio = (1.0 + sqrt 5) / 2 :: Double

digitsInFib :: Double -> Double
digitsInFib n = n * logBase 10 goldenRatio - logBase 10 (sqrt 5)

which = head . filter (\(n,m) -> m >= 999) $ [(n, digitsInFib n) | n <- [1000..10000]]

main = print $ fst which
