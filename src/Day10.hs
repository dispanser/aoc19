{-# LANGUAGE RecordWildCards #-}

module Day10 where

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List.Extra (maximumOn)
import Data.List (sortBy)
import Intcode (debugShow)

-- | the asteroid locations, modelled on a flat memory area in row-major order
data Grid = Grid { width :: Int, height :: Int, gridData :: V.Vector Bool } deriving (Eq, Show)

type Pos = (Int, Int)

-- | day 10, part 1: find maximum number of visible asteroids
--
-- >>> part1 ["..#.#", "##.##", "#...."]
-- 6
part1 :: [String] -> Int
part1 inp =
    let g = initialize inp
    in maximum $ length . visibleAsteroids g <$> asteroids g

-- | day 10, part 2: find 200th vaporized asteroid
--
--
part2 :: [String] -> Int
part2 inp =
    let grid    = initialize inp
        station = maximumOn (visibleAsteroids grid) $ asteroids grid
        go g    =
            let destroyed = destroyAsteroids g station
                g'        = clearCells g destroyed
            in if null destroyed
                  then destroyed
                  else destroyed ++ go g'
        (x,y) = go grid !! 199
    in 100*x + y

clearCells :: Grid -> [Pos] -> Grid
clearCells g px = undefined

-- | destroy visible asteroids. resulting asteroids are ordered
-- according to their bearing
--
-- >>> let g = initialize ["..#.#", "##.##", "#...."]
-- >>> destroyAsteroids g (1,4)
-- [(0,4),(2,0),(1,3),(0,2)]
--
-- >>> let g = initialize [".#....###24...#..", "##...##.13#67..9#", "##...#...5.8####.", "..#.....#...###..", "..#.#.....#....##"]
-- >>> destroyAsteroids g (3,8)
destroyAsteroids :: Grid -> Pos -> [Pos]
destroyAsteroids g st = sortBy (\p1 p2 -> compareAngle (delta p1 st) (delta p2 st)) $ visibleAsteroids g st

delta :: Pos -> Pos -> Pos
delta (x,y) (x',y') = (x-x',y-y')

-- | compare two vectors (relative to origin) by their angle w.r.t. (-1, 0)
--
-- >>> compareAngle (-1,0) (-3,1)
-- LT
-- >>> compareAngle (-1,0) (1,0)
-- LT
-- >>> compareAngle (-1,0) (-1,-1)
-- LT
-- >>> compareAngle (-1,0) (11,-1)
-- LT
-- >>> compareAngle (1,0) (11,-1)
-- LT
-- >>> compareAngle (1,0) (11,1)
-- GT
-- >>> compareAngle (-3,1) (-4, 1)
-- GT
-- >>> compareAngle (-3,1) (-2, 1)
-- LT
-- >>> compareAngle (-3,1) (-6, 2)
-- EQ
compareAngle :: Pos -> Pos -> Ordering
compareAngle (x,0) (x',0)  = compare x x'
compareAngle (x,0) (x',y')
  | x < 0 = LT
  | x > 0 = compare y' 0
compareAngle (x,y) (x',0)
  | x' < 0 = GT
  | x' > 0 = compare 0 y
compareAngle (x,y) (x',y')
  | signum y == signum y' = compare (x * y') (x' * y)


norm :: Pos -> Double
norm (x, y) = sqrt $ fromIntegral x**2 + fromIntegral y**2

dot :: Pos -> Pos -> Double
dot (x,y) (x',y') = fromIntegral (x*x' + y*y')

det' :: Pos -> Pos -> Int
det' (x,y) (x',y') = if x*y' - y*x' < 0 then -1 else 1

det :: Pos -> Pos -> Double
det (x,y) (x',y') = fromIntegral (x*y' - y*x')

-- | grid initialized from the lines represented as strings, as defined in the
-- problem description.
--
-- >>> initialize ["..#.#", "##.##", "#...."]
--Grid {width = 5, height = 3, gridData = [False,False,True,False,True,True,True,False,True,True,True,False,False,False,False]}
initialize :: [String] -> Grid
initialize xs =
    let first    = fromMaybe "" $ listToMaybe xs
        width    = length first
        height   = length xs
        gridData = V.fromList $ concatMap (( == '#') <$>) xs
    in Grid { .. }


-- | check wether there's an asteroid at a position
--
-- >>> let g = initialize ["..#.#", "##.##", "#...."]
-- >>> asteroidAt g (0,0)
-- False
-- >>> asteroidAt g (0,2)
-- True
-- >>> asteroidAt g (2,0)
-- True
asteroidAt :: Grid -> Pos -> Bool
asteroidAt g (x,y) = gridData g ! (x * width g + y)

-- | produce a list of all asteroids
--
-- >>> let g = initialize ["..#.#", "##.##", "#...."]
-- >>> asteroids g
-- [(0,2),(0,4),(1,0),(1,1),(1,3),(1,4),(2,0)]
asteroids :: Grid -> [Pos]
asteroids g@Grid{..} = [(x, y) | x <- [0.. height-1]
                       , y <- [0.. width-1]
                       , asteroidAt g (x,y)]

-- | compute the number of visible asteroids for one position
--
-- TODO: two approaches:
-- 1. for each other asteroids
--    - compute dx, dy
--    - compute greatest common divisor
--    - for all n = [1 .. (gcd - 1)]
--    - compute the x', y' and check if there's an asteroid
--    - OR those (short-circuit)
--    - if there's at least one asteroid, it's blocking the line of sight
-- 2. for each other asteroid
--    - compute all the multiples of the (dx, dy)
--    - mark those fields as invisible
--    - intersect the arrays: visible AND asteroid, filter, count.
--
-- number two seems to be more involved, we have to watch out for grid boundaries
--
-- >>> let g = initialize ["..#.#", "##.##", "#...."]
-- >>> length $ visibleAsteroids g (0,2)
-- 5
-- >>> length $ visibleAsteroids g (1,1)
-- 5
-- >>> length $ visibleAsteroids g (1,4)
-- 4
-- >>> length $ visibleAsteroids g (0,4)
-- 6
visibleAsteroids :: Grid -> Pos -> [Pos]
visibleAsteroids g pos =
    filter (/= pos) $ filter (not . blockedAsteroid g pos) $ asteroids g

-- | that signature sucks.
--
-- >>> let g = initialize ["..#.#", "##.##", "#...."]
-- >>> blockedAsteroid g (0,2) (1,1)
-- False
-- >>> blockedAsteroid g (0,2) (2,0)
-- True
-- >>> blockedAsteroid g (2,0) (0,2)
-- True
blockedAsteroid :: Grid -> Pos -> Pos -> Bool
blockedAsteroid g (x,y) (x',y') =
    let dx     = x' - x
        dy     = y' - y
        gc     = gcd dx dy
        steps  = [1 .. gc-1]
        xstep  = dx `div` gc
        ystep  = dy `div` gc
        step i = (i * xstep + x, i * ystep + y)
    in any ( asteroidAt g . step) steps
