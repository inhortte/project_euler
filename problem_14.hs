import List
import Maybe

-- n is the beginning number.
cuteSeq :: Int -> [Int]
cuteSeq n | n <= 1 = [n]
          | even n = n : cuteSeq (n `quot` 2)
          | odd n = n : cuteSeq (3*n + 1)

-- m is the max to begin with.
-- longSeq' -> startnum longest length
{--
longSeq :: Integer -> Integer
longSeq m | m < 1 = error "You damned sloth!"
          | otherwise = longSeq' 1 1 1
          where longSeq' st long len | st == m = long
                                     | newLength > len = longSeq' (st+1) st newLength
                                     | otherwise = longSeq' (st+1) long len
                                     where newLength = length (cuteSeq st)
--}

-- but stack overflow for 1000000.
longSeq :: Int -> Int
longSeq m | m < 1 = error "You damned sloth!"
          | otherwise = (fromMaybe 0 (elemIndex (maximum lengths) lengths)) + 1
          where lengths = map (length . cuteSeq) [1..m]

