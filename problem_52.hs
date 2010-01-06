import StringInt
import Data.List (permutations)
import List

nums = [x | x <- [10,20..10000000], permEtc x]

permEtc :: Integer -> Bool
permEtc x | allPerms quots perms = True
          | otherwise          = False
          where perms = permutations $ show x
                quots = map (show . (x `quot`)) [2..6]
                allPerms qs pms = (==) 5 $ length $ filter (\q -> q `elem` pms) qs

--------
-- better solution?

arePerms :: Integer -> Integer -> Bool
arePerms a b = (show a) \\ (show b) == []

hasPerms :: Integer -> Bool
hasPerms x = all (x `arePerms`) $ map (*x) [2..6]

problem_52 = head $ filter hasPerms [1..]
