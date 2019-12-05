{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import qualified Intcode as I
import qualified Data.Vector.Generic as G
import Data.Vector.Generic ((//))

part1 :: G.Vector v Int => v Int -> Int
part1 = I.eval

-- | day 2, part 2: iterate a space of potential inputs to produce a specific result
part2 :: G.Vector v Int => v Int -> Int -> [Int]
part2 ic expected =
    [ 100 * noun + verb | noun <- [0..99],
                          verb <- [0..99],
                          I.eval (ic // [(1, noun), (2, verb)]) == expected]

