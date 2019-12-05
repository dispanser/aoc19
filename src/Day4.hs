module Day4 where

import Data.List (isInfixOf)

search :: (Int -> Bool) -> Int -> Int -> Int
search pred lower upper = length $ filter pred [lower .. upper]

part1 :: Int -> Int -> Int
part1 = search isValidPart1

part2 :: Int -> Int -> Int
part2 = search isValidPart2

-- | Day 4, part 2: check if a number is valid according
-- to the criteria described in the problem description
--
-- >>> isValidPart2 111111
-- False
-- >>> isValidPart2 223450
-- False
-- >>> isValidPart2 123789
-- False
-- >>> isValidPart2 112233
-- True
-- >>> isValidPart2 123444
-- False
-- >>> isValidPart2 111122
-- True
isValidPart2 :: Int -> Bool
isValidPart2 n =
    let ds             = digits n
        adjacentEquals = False : zipWith (==) ds (tail ds) ++ [False]
        exactlyTwoEq   = [False, True, False] `isInfixOf` adjacentEquals
        nonDecreasing  = and . zipWith (<=) ds $ tail ds
    in nonDecreasing && exactlyTwoEq

-- | Day 4, part 1: check if a number is valid according
-- to the criteria described in the problem description
--
-- >>> isValidPart1 111111
-- True
-- >>> isValidPart1 223450
-- False
-- >>> isValidPart1 123789
-- False
isValidPart1 :: Int -> Bool
isValidPart1 n =
    let ds                = digits n
        twoAdjacentEquals = or . zipWith (==) ds $ tail ds
        nonDecreasing     = and . zipWith (<=) ds $ tail ds
    in twoAdjacentEquals && nonDecreasing

-- | split a number into a list of digits
--
-- >>> digits 1231
-- [1,2,3,1]
digits :: Int -> [Int]
digits = go
 where go 0 = []
       go n = let lastDigit = n `mod` 10
              in digits (n `div` 10) ++ [lastDigit]
