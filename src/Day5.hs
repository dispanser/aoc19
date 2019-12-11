module Day5 where

import qualified Intcode as I

part1 :: [Int] -> [Int] -> [I.Action]
part1 vs inp = fst . I.runProgram $ I.initialize vs `I.withInputs` inp

part2 :: [Int] -> [Int] -> [I.Action]
part2 vs inp = fst . I.runProgram $ I.initialize vs `I.withInputs` inp
