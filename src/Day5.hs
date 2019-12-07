module Day5 where

import qualified Data.Vector.Unboxed as V
import qualified Intcode as I

part1 :: V.Vector Int -> [Int] -> [I.Action]
part1 vs inp = fst . I.runProgram $ I.initialize vs `I.withInputs` inp

part2 :: V.Vector Int -> [Int] -> [I.Action]
part2 vs inp = fst . I.runProgram $ I.initialize vs `I.withInputs` inp
