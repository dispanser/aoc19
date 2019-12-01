module AdventOfCode where

import System.IO (readFile)

import qualified D1M2

main :: IO ()
main = do
    ls <- lines <$> readFile "data/d1m1.txt"
    let ws = (read :: String -> Int) <$> ls
    print . sum $ map D1M2.fuel ws

