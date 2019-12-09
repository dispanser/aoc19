module Day6 where

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

import Data.Maybe (isJust, catMaybes)
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
part1 inputs = HM.foldl' (+) 0 $ orbits
  where parentRel = parseInput inputs
        orbits    = computeOrbits parentRel

-- | part 2
--
-- >>> let parents = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]
-- >>> part2 parents
-- 4
part2 :: [String] -> Int
part2 inputs =
    let parentRel = parseInput inputs
    in distance "SAN" "YOU" parentRel

-- | compute the distance between two given map entries in the DAG
--
-- >>> let parents = parseInput ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]
-- >>> distance "SAN" "YOU" parents
-- 4
distance :: String -> String -> HM.HashMap String String -> Int
distance from to parents =
    let fromRootPath = rootPath parents from
        toRootPath   = rootPath parents to
        commonLength = length . takeWhile id $ zipWith (==) fromRootPath toRootPath
    in length fromRootPath + length toRootPath - 2*commonLength - 2

-- | find the path from a specific entry to the root of the DAG
--
-- >>> let parents = parseInput ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]
-- >>> rootPath parents "COM"
-- ["COM"]
-- >>> rootPath parents "L"
-- ["COM","B","C","D","E","J","K","L"]
rootPath :: HM.HashMap String String -> String -> [String]
rootPath parents from = reverse . catMaybes . takeWhile isJust $ iterate (>>= flip HM.lookup parents) (Just from)

-- >>> HM.toList $ parseInput ["COM)ABC", "ABC)DEF"]
-- [("COM",0),("ABC",1), ("DEF", 2)]
computeOrbits :: HM.HashMap String String -> HM.HashMap String Int
computeOrbits parentRel =
    let orbits = HM.singleton "COM" 0 <> HM.map (\k -> 1 + (orbits ! k)) parentRel
    in orbits

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
