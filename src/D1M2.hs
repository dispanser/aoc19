module D1M2 where

import qualified D1M1

fuel :: Int -> Int
fuel = sum . takeWhile (>= 0) . iterate D1M1.fuel . D1M1.fuel

