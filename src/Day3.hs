{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Day3 where

import Data.List (sortOn)
import qualified Data.List.Split as S
import Data.Hashable (Hashable)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
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
-- >>> part1 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
-- 159
-- >>> part1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
-- 135
part1 :: String -> String -> Int
part1 wire1 wire2 =
    let wire1Cells = HS.fromList . map fst . tilesOnPath $ parseWirePath wire1
        wire2Cells = HS.fromList . map fst . tilesOnPath $ parseWirePath wire2
        crossings  = HS.intersection wire1Cells wire2Cells
        distances  = HS.map (manhattan (Pos 0 0)) crossings
    in minimum distances

-- >>> part2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83"
-- 610
-- >>> part2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
-- 410
part2 :: String -> String -> Int
part2 wire1 wire2 =
    let wireCells1 = distancePerCell . tilesOnPath $ parseWirePath wire1
        wireCells2 = distancePerCell . tilesOnPath $ parseWirePath wire2
        crossings  = HS.toList $ HS.intersection (HM.keysSet wireCells1) (HM.keysSet wireCells2)
        weights k  = wireCells1  HM.! k + wireCells2 HM.! k
        distances  = weights <$> crossings
    in minimum distances

-- | create a map from each distinct position to the smallest distance
-- TODO: this seems very generic, adapt type signature
--
-- >>> distancePerCell [(Pos {x = 0, y = -1},1),(Pos {x = 0, y = -2},7),(Pos {x = 0, y = -1},3),(Pos {x = 0, y = -2},4)]
-- fromList [(Pos {x = 0, y = -2},4),(Pos {x = 0, y = -1},1)]
distancePerCell :: [(Pos, Int)] -> HM.HashMap Pos Int
distancePerCell = foldl' (\hm (k, v) -> HM.insertWith min k v hm) HM.empty

-- | manhattan distance: distance on grid
--
-- >>> manhattan (Pos 3 4) (Pos 4 3)
-- 2
-- >>> manhattan (Pos 3 4) (Pos 4 8)
-- 5
manhattan :: Pos -> Pos -> Int
manhattan (Pos x y) (Pos x' y') = abs (x - x') + abs (y - y')

-- | produce a list of visited cells in order of traversal
--
-- >>> tilesOnPath [D 2, L 2]
-- [(Pos {x = 0, y = -1},1),(Pos {x = 0, y = -2},2),(Pos {x = -1, y = -2},3),(Pos {x = -2, y = -2},4)]
tilesOnPath :: [Move] -> [(Pos, Int)]
tilesOnPath = go 0 (Pos 0 0)
  where
    go wireLen startPos (m:ms) =
        let (endPos, pss) = cellsInMove startPos m
            indexedPos    = pss `zip` [wireLen + 1 .. ]
        in indexedPos ++ go (wireLen + length pss) endPos ms
    go _ _ [] = []
-- | compute the set of visited cells for a move starting at a given position
--
-- >>> cellsInMove (Pos 2 1) (D 2)
-- (Pos {x = 2, y = -1},[Pos {x = 2, y = 0},Pos {x = 2, y = -1}])
cellsInMove :: Pos -> Move -> (Pos, [Pos])
cellsInMove start@Pos{..} move =
    let dest@(Pos x' y') = moveDestination start move
        minX = min x x'
        maxX = max x x'
        minY = min y y'
        maxY = max y y'
        cells = [Pos vx vy | vx <- [minX .. maxX], vy <- [minY .. maxY]]
    in (dest, tail $ sortOn (manhattan start) cells)

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
