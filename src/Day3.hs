{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Day3 where

import Data.List (sortOn)
import qualified Data.List.Split as S
import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import GHC.Generics (Generic)
import Data.Foldable (foldl')

-- | Day 3 - Crossing Wires
--
-- A relatively simple (but brute force) solution is to generate all positions
-- for both wires, and then just sort them by their manhatten distance.

data Move = L Int
              | R Int
              | U Int
              | D Int
              deriving (Eq, Show)

data Pos = Pos { x :: Int
               , y :: Int } deriving (Eq, Show, Generic)

instance Hashable Pos

-- |
--
-- >>> solve "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
-- 159
-- >>> solve "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
-- 135
solve :: String -> String -> Int
solve wire1 wire2 =
    let wire1Cells = tilesOnPath $ parseWirePath wire1
        wire2Cells = tilesOnPath $ parseWirePath wire2
        crossings  = HS.delete (Pos 0 0) $ HS.intersection wire1Cells wire2Cells
        distances  = HS.map (manhattan (Pos 0 0)) crossings
    in minimum distances

-- | manhattan distance: distance on grid
--
-- >>> manhattan (Pos 3 4) (Pos 4 3)
-- 2
-- >>> manhattan (Pos 3 4) (Pos 4 8)
-- 5
manhattan :: Pos -> Pos -> Int
manhattan (Pos x y) (Pos x' y') = abs (x - x') + abs (y - y')

tilesOnPath :: [Move] -> HS.HashSet Pos
tilesOnPath = go (Pos 0 0) HS.empty
  where
    go startPos aggr (m:ms) =
        let (endPos, pss) = cellsInMove startPos m
            aggr'         = foldl' (flip HS.insert) aggr pss
        in go endPos aggr' ms
    go _ aggr []            = aggr
-- | compute the set of visited cells for a move starting at a given position
--
-- >>> cellsInMove (Pos 2 1) (D 2)
-- (Pos {x = 2, y = -1},[Pos {x = 2, y = 1},Pos {x = 2, y = 0},Pos {x = 2, y = -1}])
cellsInMove :: Pos -> Move -> (Pos, [Pos])
cellsInMove start@Pos{..} move =
    let dest@(Pos x' y') = moveDestination start move
        minX = min x x'
        maxX = max x x'
        minY = min y y'
        maxY = max y y'
        cells = [Pos vx vy | vx <- [minX .. maxX], vy <- [minY .. maxY]]
    in (dest, sortOn (manhattan start) cells)

-- | compute the end position for a given a start position and a move.
--
-- >>> moveDestination (Pos 1 2) (L 4)
-- Pos {x = -3, y = 2}
-- >>> moveDestination (Pos (-1) 2) (R 4)
-- Pos {x = 3, y = 2}
-- >>> moveDestination (Pos (-1) 2) (D 4)
-- Pos {x = -1, y = -2}
moveDestination :: Pos -> Move -> Pos
moveDestination (Pos x y) = \case
  D dy -> Pos x $ y - dy
  U dy -> Pos x $ y + dy
  L dx -> Pos (x - dx) y
  R dx -> Pos (x + dx) y

-- | parse a string representing a wire path into a sequence of moves
--
-- >>> parseWirePath "R991,U847,L239,U883,L224"
-- [R 991,U 847,L 239,U 883,L 224]
parseWirePath :: String -> [Move]
parseWirePath s = parseMove <$> S.splitOn "," s

-- | parse an individual move
--
-- >>> parseMove "D123"
-- D 123
-- >>> parseMove "L1"
-- L 1
-- >>> parseMove "U13"
-- U 13
-- >>> parseMove "R01"
-- R 1
parseMove :: String -> Move
parseMove m =
    let len = read $ tail m
    in case head m of
      'L' -> L len
      'R' -> R len
      'U' -> U len
      'D' -> D len
