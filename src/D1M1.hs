module D1M1 where

import System.IO (readFile)

main :: IO ()
main = do
    ls <- lines <$> readFile "data/d1m1.txt"
    let ws = (read :: String -> Int) <$> ls
    print . sum $ map fuel ws

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)
