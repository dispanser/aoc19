module AdventOfCode where

import System.IO (readFile)

import qualified D1M2
import qualified D1M1
import qualified Data.List.Split as S
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import Data.Vector.Unboxed (fromList, (//))

main :: IO ()
main = do
    day1
    day2
    day3
    day4


day1 :: IO ()
day1 = do
    ws <- readIntLines "data/d1m1.txt"
    putStrLn $ "day 1, part #1: " ++ show (sum $ map D1M1.fuel ws)
    putStrLn $ "day 1, part #2: " ++ show (sum $ map D1M2.fuel ws)

day2 :: IO ()
day2 = do
    intCodeIn  <- fromList <$> readIntList "data/day2.txt"
    let intCodeMod = intCodeIn // [(1, 12), (2, 2)]
    putStrLn $ "day 2, part #1: " ++ show (Day2.part1 intCodeMod)
    putStrLn $ "day 2, part #2: " ++ show (Day2.part2 intCodeIn 19690720)

day3 :: IO ()
day3 = do
    wires <- lines <$> readFile "data/day3.txt"
    putStrLn $ "day 3, part #1: " ++ show ( Day3.part1 (head wires) (wires !! 1))
    putStrLn $ "day 3, part #2: " ++ show ( Day3.part2 (head wires) (wires !! 1))

day4 :: IO ()
day4 = do
    putStrLn $ "day4, part #1: " ++ show (Day4.part1 245182 790572)
    putStrLn $ "day4, part #2: " ++ show (Day4.part2 245182 790572)

day5 :: IO ()
day5 = do
    intCodeIn  <- fromList <$> readIntList "data/day5.txt"
    putStrLn $ "day 5, part #1: " ++ show (Day5.part1 intCodeIn [1])

readIntLines :: FilePath -> IO [Int]
readIntLines f = do
    ls <- lines <$> readFile f
    return $ read <$> ls

readIntList :: FilePath -> IO [Int]
readIntList f = do
    ints <- S.splitOn "," <$> readFile f
    return $ read <$> ints

