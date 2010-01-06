import Primes
import Data.List

oddComposites :: [Integer]
oddComposites = [x | x <- [3,5..], (not . isPrime) x]

goldbachWasWrong :: [Integer]
goldbachWasWrong = filter doIt oddComposites
    where doIt n = not .
                   any isPrime .
                   takeWhile (>0) .
                   map (\x -> n - 2 * x * x) $ [1..]
