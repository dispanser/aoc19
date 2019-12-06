{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import qualified Intcode as I
import qualified Data.Vector.Unboxed as V
import Data.Vector.Generic ((//))

part1' :: V.Vector Int -> Int
part1' = snd . I.runProgram

part2' :: V.Vector Int -> Int -> [Int]
part2' ic expected =
    [ 100 * noun + verb | noun <- [0..99],
                          verb <- [0..99],
                          snd (I.runProgram (ic // [(1, noun), (2, verb)])) == expected]
