{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import qualified Intcode as I

part1 :: [Int] -> Int
part1 = snd . I.runProgram . I.initialize

part2 :: [Int] -> Int -> [Int]
part2 ic expected =
    let first      = head ic
        rest       = drop 3 ic
        initIc n v = I.initialize $ first : n : v : rest
    in [ 100 * noun + verb | noun <- [0..99],
                             verb <- [0..99],
                             snd (I.runProgram $ initIc noun verb) == expected]
