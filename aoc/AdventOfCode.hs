module AdventOfCode where

import System.IO (readFile)

import qualified D1M2
import qualified D1M1
import qualified Data.List.Split as S
import qualified Day2
import Data.Vector.Unboxed (fromList, (//))

main :: IO ()
main = do
    day1


day1 :: IO ()
day1 = do
    ws <- readIntLines "data/d1m1.txt"
    putStrLn $ "day 1, part #1: " ++ (show $ sum $ map D1M1.fuel ws)
    putStrLn $ "day 1, part #2: " ++ (show $ sum $ map D1M2.fuel ws)

day2 :: IO ()
day2 = do
    intCodeIn  <- fromList <$> readIntList "data/day2.txt"
    let intCodeMod = intCodeIn // [(1, 12), (2, 2)]
    putStrLn $ "day 2, part #1: " ++ (show $ Day2.solve intCodeMod)
    putStrLn $ "day 2, part #2: " ++ (show $ Day2.findInputs intCodeIn 19690720)

readIntLines :: FilePath -> IO [Int]
readIntLines f = do
    ls <- lines <$> readFile f
    return $ read <$> ls

readIntList :: FilePath -> IO [Int]
readIntList f = do
    ints <- S.splitOn "," <$> readFile f
    return $ read <$> ints
