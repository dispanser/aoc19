module D1M1 where

fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)
