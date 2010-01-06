divisors :: Integer -> [Integer]
divisors n | n < 1 = error "huh?"
           | otherwise = [d | d <- [1..(n `quot` 2)], n `mod` d == 0] ++ [n]

maxDivisors :: [Integer] -> (Integer, Integer)
maxDivisors [] = error "huh?"
maxDivisors ns = maxDivisors' (1, 1) ns
    where maxDivisors' (highest, amount) [] = (highest, amount)
          maxDivisors' (highest, amount) (n:ns) =
              maxDivisors' (if ds > amount
                            then (n, ds)
                            else (highest, amount)) ns
                  where ds = (toInteger . length . divisors) n
