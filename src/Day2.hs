{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import qualified Intcode as I
import qualified Data.Vector.Unboxed as V
import Data.Vector.Generic ((//))

part1 :: V.Vector Int -> Int
part1 = snd . I.runProgram . I.initialize

part2 :: V.Vector Int -> Int -> [Int]
part2 ic expected =
    let initIc n v = I.initialize (ic // [(1, n), (2, v)])
    in [ 100 * noun + verb | noun <- [0..99],
                             verb <- [0..99],
                             snd (I.runProgram $ initIc noun verb) == expected]
