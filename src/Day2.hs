{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Day2 where

import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed ((!), (//))
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

solve :: Intcode -> Int
solve = go 0
 where go p ic = case evalAt p ic of
                   Halt -> V.head ic
                   m    -> go (p+4) (applyMod ic m)

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
evalAt :: Int -> Intcode -> Mod
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

applyMod :: Intcode -> Mod -> Intcode
applyMod ic Halt = ic
applyMod ic Mod { .. } = ic // [(pos, val)]

debugShow :: Show a => String -> a -> a
debugShow prefix v =
    let msg = prefix ++ " " ++ show v
    in trace msg v

