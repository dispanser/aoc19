module Day6 where

import Debug.Trace (trace, traceShow)

-- Ok, getting started:
-- - this is a DAG
-- - we need to represent the parent / child relationship
-- - orbits(x) = 1 + orbits(parent(x))
-- - solution: sum over all orbits
-- simplest thing that could work
-- - Map [String, String]: parent
-- - map lookup: Nothing -> orbit 0
-- - map lookup: Just x -> orbit x + 1
-- - this does not cache, so we want to look into some DP or memoization
-- - what's lazy evaluation good for, anyway?
-- -- another maps: Map[String, Int]... this could be initially filled with thunks,
--    now that seems kinda nice

import Data.List.Split (splitOn)
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy as HM ((!))


-- TODO: use optics for no good reason
--
-- | compute the number of direct and indirect orbits
--
-- >>> part1 []
-- 0
-- >>> part1 ["COM)ABC"]
-- 1
-- >>> part1 ["COM)ABC", "ABC)DEF"]
-- 3
-- >>> part1 ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]
-- 42
part1 :: [String] -> Int
part1 inputs = HM.foldl' (+) 0 orbits
  where parentRel = parseInput inputs
        orbits :: HM.HashMap String Int
        orbits = HM.singleton "COM" 0 <> HM.map (\k -> 1 + (orbits ! k)) parentRel

-- | parse inputs of day 6, list of strings of the form "xyz)abc" where abc orbits around xyz.
--
-- >>> parseInput []
-- fromList []
-- >>> parseInput ["COM)ABC"]
-- fromList [("ABC","COM")]
-- >>> HM.toList $ parseInput ["COM)ABC", "ABC)DEF"]
-- [("DEF","ABC"),("ABC","COM")]
parseInput :: [String] -> HM.HashMap String String
parseInput inputs = HM.fromList $ parseLine <$> inputs
  where parseLine l = let [child,parent] = splitOn ")" l
                      in (parent, child)

debugShow :: Show a => String -> a -> a
debugShow prefix v =
    let msg = prefix ++ " " ++ show v
    in trace msg v
