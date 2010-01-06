decToBin x = reverse $ decToBin' x
    where decToBin' 0 = []
          decToBin' y = [b] ++ decToBin' a
              where (a,b) = quotRem y 2

palindrome xs = reverse xs == xs

palNums = [x | x <- [1..1000000], (palindrome $ show x) && (palindrome $ decToBin x)]

