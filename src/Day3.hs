{-# LANGUAGE LambdaCase #-}

module Day3 where

import qualified Data.List.Split as S

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
               , y :: Int } deriving (Eq, Show)

visitedTiles :: Pos -> Move -> (Pos, [Pos])
visitedTiles start move = undefined

-- | given a start position and a move, compute the end position
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
