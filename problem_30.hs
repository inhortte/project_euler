import Data.Char (digitToInt)

-- m == maximum, w == where.
fifthPower :: Int -> [Int]
fifthPower m = fifthPower' 2 m []
    where
      fifthPower' w m numList | w > m = numList
                              | w == fifthPows = fifthPower' (w+1) m (numList ++ [w])
                              | otherwise = fifthPower' (w+1) m numList
                              where fifthPows = sum $ map (^5) $ map digitToInt $ show w

-- or
ans = sum [x | x <- [2..999999], x == (sum $ map (^5) $ map digitToInt $ show x)]
