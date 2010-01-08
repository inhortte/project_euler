import Data.List (elemIndex, maximumBy)
import Data.Ord  (comparing)

repCycle :: Int -> Int
repCycle d = rems d 10 []

rems :: Int -> Int -> [Int] -> Int
rems d 0 rs = 0
rems d r rs   = case rem `elemIndex` rs of
                  Just n  -> n + 1
                  Nothing -> rems d (10 * rem) (rem : rs)
    where rem = r `mod` d

ans = maximumBy (comparing snd) $ [(n, repCycle n) | n <- [1..999]]
