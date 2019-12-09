module Day8 where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Char (digitToInt, intToDigit)
import Data.List.Split (chunksOf)
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

newtype Layer = Layer {unlayer :: V.Vector Int} deriving (Eq, Show)


-- |
-- >>> part1 2 3 "034122001122"
-- 2
-- >>> part1 2 3 "000000"
-- 0
-- >>> part1 2 3 "011222123321"
-- 4
part1 :: Int -> Int -> String -> Int
part1 rows cols inp =
    let layers    = decodeStream rows cols inp
        zeroLayer = head $ sortOn (countDigit 0) layers
    in countDigit 1 zeroLayer * countDigit 2 zeroLayer

-- | Day 8, part 2: find the defining pixel (ignoring transparent ones)
--
-- >>> part2 2 2 "0222112222120000"
-- "0110"
part2 :: Int -> Int -> String -> String
part2 rows cols inp =
    let layers   = decodeStream rows cols inp
        pixels n = (! n) . unlayer <$> layers
        pixel    = fromMaybe 2 . listToMaybe . filter (/= 2)
    in intToDigit . pixel . pixels <$> [0 .. rows * cols - 1]
-- | count occurrences of given digit in the layer
--
-- >>> countDigit 5 $ Layer $ V.fromList [5,5,4,1,1,1]
-- 2
-- >>> countDigit 0 $ Layer $ V.fromList [5,5,4,1,1,1]
-- 0
-- >>> countDigit 1 $ Layer $ V.fromList [5,5,4,1,1,1]
-- 3
countDigit :: Int -> Layer -> Int
countDigit digit = V.length . V.filter (==digit) . unlayer

-- |
--
-- >>> decodeStream 2 3 "123456789012"
-- [Layer {unlayer = [1,2,3,4,5,6]},Layer {unlayer = [7,8,9,0,1,2]}]
decodeStream :: Int -> Int -> String -> [Layer]
decodeStream rows cols inp = decodeSingle <$> chunksOf (rows * cols) inp
 where
  decodeSingle :: String -> Layer
  decodeSingle xs = Layer . V.fromList $ digitToInt <$> xs


