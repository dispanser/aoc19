module Day9 where

import qualified Intcode as I

part1 :: [Int] -> String
part1 mem = show $ I.runProgram $ I.initialize mem `I.withInputs` [1]

part2 :: [Int] -> String
part2 mem = show $ I.runProgram $ I.initialize mem `I.withInputs` [2]
