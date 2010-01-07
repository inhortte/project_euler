module GeometricNumbers
    where

import qualified Data.Set as Set

-- isTypeOfNumber :: Integer -> [Integer] -> Bool
-- isTypeOfNumber n t = (==) n $ last $ takeWhile (<=n) t

isTypeOfNumber :: Integer -> [Integer] -> Bool
isTypeOfNumber n t = (Set.member n . Set.fromList . takeWhile (<=n)) t

triangleNumber :: Integer -> Integer
triangleNumber n = n * (n + 1) `quot` 2

triangleNumbers :: [Integer]
triangleNumbers = map triangleNumber [1..]

isTriangleNumber :: Integer -> Bool
isTriangleNumber n = isTypeOfNumber n triangleNumbers

pentagonalNumber :: Integer -> Integer
pentagonalNumber n = n * (3*n - 1) `div` 2

pentagonalNumbers :: [Integer]
pentagonalNumbers = map pentagonalNumber [1..]

isPentagonalNumber :: Integer -> Bool
isPentagonalNumber n = isTypeOfNumber n pentagonalNumbers

hexagonalNumber :: Integer -> Integer
hexagonalNumber n = n * (2*n - 1)

hexagonalNumbers :: [Integer]
hexagonalNumbers = [hexagonalNumber n | n <- [1..]]

isHexagonalNumber :: Integer -> Bool
isHexagonalNumber n = isTypeOfNumber n hexagonalNumbers

