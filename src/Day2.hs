{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import qualified Data.Vector.Generic as G
import Data.Vector.Generic ((!), (//))
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace)

type Intcode = V.Vector Int
type MIntcode s = STVector s Int

data Mod = Mod { pos :: Int
               , val :: Int }
         | Halt deriving (Show, Eq)

-- | evaluate a program until it halts
--
-- >>> solve (V.fromList [1,9,10,3,2,3,11,0,99,30,40,50])
-- 3500
-- >>> solve (V.fromList [1,0,0,0,99])
-- 2
-- >>> solve (V.fromList [1,1,1,4,99,5,6,0,99])
-- 30
solve :: G.Vector v Int => v Int -> Int
solve = go 0
 where go p ic = case evalAt p ic of
                   Halt -> G.head ic
                   m    -> go (p+4) (applyMod ic m)

-- | day 2, part 2: iterate a space of potential inputs to produce a specific result
findInputs :: G.Vector v Int => v Int -> Int -> [Int]
findInputs ic expected =
    [ 100 * noun + verb | noun <- [0..99],
                          verb <- [0..99],
                          solve (ic // [(1, noun), (2, verb)]) == expected]

-- | evaluate an @intcode@ program at a specific position, and produce a result
--
-- >>> evalAt 0 $ V.fromList [99]
-- Halt
-- >>> evalAt 1 $ V.fromList [-1, 99, 98, 3, 87]
-- Halt
-- >>> evalAt 0 $ V.fromList [1, 5, 3, 3, -1, 99]
-- Mod {pos = 3, val = 102}
-- >>> evalAt 0 $ V.fromList [2, 5, 3, 4, -1, 99]
-- Mod {pos = 4, val = 396}
evalAt :: G.Vector v Int => Int -> v Int -> Mod
evalAt at ic =
    let op  = ic ! at
        v1  = ic ! (ic ! (at + 1))
        v2  = ic ! (ic ! (at + 2))
        pos = ic ! (at + 3)
    in case op of
         1  -> Mod pos $ v1 + v2
         2  -> Mod pos $ v1 * v2
         99 -> Halt
         o  -> error $ "unexpected op code" ++ show o

applyMod :: G.Vector v Int => v Int -> Mod -> v Int
applyMod ic Halt = ic
applyMod ic Mod { .. } = ic // [(pos, val)]

debugShow :: Show a => String -> a -> a
debugShow prefix v =
    let msg = prefix ++ " " ++ show v
    in trace msg v

