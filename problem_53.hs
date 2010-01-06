import Data.List ((\\), intersect)

comb :: Integer -> Integer -> Integer
comb n r | n < 1 || r < 1 || r > n = error "You fucked it up."
         | otherwise = (product n_minus_r_list) `quot` (product [1..(n-r)])
         where n_list = [1..n]
               r_list = [1..r]
               n_minus_r_list = n_list \\ r_list

ans = length [x | n <- [1..100], r <- [1..n], let x = comb n r, x > 1000000]
