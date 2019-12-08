module Day8 where

import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Data.List (sortOn)
import qualified Data.Vector.Unboxed as V

newtype Layer = Layer [V.Vector Int] deriving (Eq, Show)

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

-- | count occurrences of given digit in the layer
--
-- >>> countDigit 5 $ Layer [V.fromList [5,5,4],V.fromList [1,1,1]]
-- 2
-- >>> countDigit 0 $ Layer [V.fromList [5,5,4],V.fromList [1,1,1]]
-- 0
-- >>> countDigit 1 $ Layer [V.fromList [5,5,4],V.fromList [1,1,1]]
-- 3
countDigit :: Int -> Layer -> Int
countDigit digit (Layer ls) = sum $ V.length . V.filter (==digit) <$> ls

-- |
--
-- >>> decodeStream 2 3 "123456789012"
-- [Layer [[1,2,3],[4,5,6]],Layer [[7,8,9],[0,1,2]]]
decodeStream :: Int -> Int -> String -> [Layer]
decodeStream rows cols inp = decodeSingle cols <$> chunksOf (rows * cols) inp

-- |
--
-- >>> decodeSingle 3 "654321"
-- Layer [[6,5,4],[3,2,1]]
decodeSingle :: Int -> String -> Layer
decodeSingle cols inp = Layer $ parseLayerLine <$> chunksOf cols inp
  where parseLayerLine :: String -> V.Vector Int
        parseLayerLine l = V.fromList $ digitToInt <$> l
