{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Intcode where

import qualified Data.Vector.Generic as G
import Data.Vector.Generic ((!), (//))
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace, traceShow)

-- | the state of the intcode interpreter: memory and instruction pointer
data IState = IState { mem :: V.Vector Int
                     , ip  :: Int } deriving (Eq, Show)

data Action  = Terminate | Output Int | None deriving (Eq, Show)

-- | interpret directly as a value or as a memory address
data Mode  = Direct | MemAddr deriving (Show, Eq)

-- | function parameter is a value that is either interpreted as a mem address
-- or directly as a value
data Param = Param { mode  :: Mode
                   , value :: Int } deriving (Show, Eq)

data Op = Add { p1  :: Param
              , p2  :: Param
              , out :: Param }
        | Mul { p1  :: Param
              , p2  :: Param
              , out :: Param }
        | Store { p1 :: Param }
        | Read  { p1 :: Param }
        | Term
        deriving (Eq, Show)

initialize :: V.Vector Int -> IState
initialize = flip IState 0

-- | run an @intcode@ program, return the list of actions
--
-- >>> runProgram (V.fromList [1,11,12,3,2,3,13,0,4,0,99,30,40,50])
-- [Output 3500,Terminate]
-- >>> runProgram (V.fromList [1,0,0,0,4,0,99])
-- [Output 2,Terminate]
-- >>> runProgram (V.fromList [1,1,1,4,99,5,6,0,4,0,99])
-- [Output 30,Terminate]
runProgram :: V.Vector Int -> [Action]
runProgram ic = go start
 where start = initialize ic
       go is = let (action, newState) = step is
                   next = go newState
               in case action of
                    Terminate -> [Terminate]
                    None      -> next
                    a         -> a : next

-- | advancing an intcode programm by one step.
--
-- >>> step $ initialize $ V.fromList [99]
-- (Terminate,IState {mem = [99], ip = 1})
-- >>> step $ initialize $ V.fromList [1, 3, 2, 1]
-- (None,IState {mem = [1,3,2,1], ip = 4})
-- >>> step $ initialize $ V.fromList [2, 3, 2, 1]
-- (None,IState {mem = [2,2,2,1], ip = 4})
-- >>> step $ initialize $ V.fromList [1,9,10,3,2,3,11,0,4,0,99,30,40,50]
-- (None,IState {mem = [1,9,10,99,2,3,11,0,4,0,99,30,40,50], ip = 4})
step :: IState -> (Action, IState)
step is@IState {..} =
    let (ip', instruction) = readOp is
    in case instruction of
         Term       -> (Terminate, is { ip = ip' })
         Add { .. } -> let newVal = evalParam is p1 + evalParam is p2
                           mem'   = mem // [(value out, newVal)]
                       in (None, IState { ip = ip' , mem = mem'})
         Mul { .. } -> let newVal = evalParam is p1 * evalParam is p2
                           mem'   = mem // [(value out, newVal)]
                       in (None, IState { ip = ip' , mem = mem'})
         Read { .. } -> let outVal = evalParam is p1
                        in (Output outVal, is { ip = ip' })

-- | read a parameter from a program
--
-- >>> evalParam (initialize $ V.fromList [1, 3, 2, 1]) $ Param MemAddr 3
-- 1
-- >>> evalParam (initialize $ V.fromList [1, 3, 2, 1]) $ Param Direct 3
-- 3
evalParam :: IState -> Param -> Int
evalParam is (Param MemAddr value) = mem is ! value
evalParam _  (Param Direct value ) = value

-- | read operation from a machine state
--
-- >>> readOp $ initialize $ V.fromList [99]
-- (1,Term)
-- >>> readOp $ initialize $ V.fromList [1, 3, 2, 1]
-- (4,Add {p1 = Param {mode = MemAddr, value = 3}, p2 = Param {mode = MemAddr, value = 2}, out = Param {mode = MemAddr, value = 1}})
-- >>> readOp $ initialize $ V.fromList [2, 3, 2, 1]
-- (4,Mul {p1 = Param {mode = MemAddr, value = 3}, p2 = Param {mode = MemAddr, value = 2}, out = Param {mode = MemAddr, value = 1}})
-- >>> readOp $ initialize $ V.fromList [1002, 3, 80, 1]
-- (4,Mul {p1 = Param {mode = MemAddr, value = 3}, p2 = Param {mode = Direct, value = 80}, out = Param {mode = MemAddr, value = 1}})
readOp :: IState -> (Int, Op)
readOp IState { .. } =
    let (mode, opcode) = (mem ! ip) `divMod` 100
    in case opcode of
         99 -> (ip + 1, Term)
         1  -> (ip + 4, Add
             { p1 =  Param (paramMode mode 0) $ mem ! (ip + 1)
             , p2 =  Param (paramMode mode 1) $ mem ! (ip + 2)
             , out = Param (paramMode mode 2) $ mem ! (ip + 3) })
         2  -> (ip + 4, Mul
             { p1 =  Param (paramMode mode 0) $ mem ! (ip + 1)
             , p2 =  Param (paramMode mode 1) $ mem ! (ip + 2)
             , out = Param (paramMode mode 2) $ mem ! (ip + 3) })
         3  -> error "unsupported operation: Store"
         4  -> (ip + 2, Read $ Param (paramMode mode 0) $ mem ! (ip + 1))
         -- _  -> error "unsupported operation: " ++ show opcode

paramMode :: Int -> Int -> Mode
paramMode mode pos =
    let shiftRight = mode `div` 10 ^ pos
    in case shiftRight `mod` 10 of
         0 -> MemAddr
         1 -> Direct

-- | result of an operation: change the value of a specific cell
data Mod = Mod { pos :: Int
               , val :: Int }
         | Halt deriving (Show, Eq)

-- | evaluate a program until it halts
--
-- >>> eval (V.fromList [1,9,10,3,2,3,11,0,99,30,40,50])
-- 3500
-- >>> eval (V.fromList [1,0,0,0,99])
-- 2
-- >>> eval (V.fromList [1,1,1,4,99,5,6,0,99])
-- 30
eval :: G.Vector v Int => v Int -> Int
eval = go 0
 where go p ic = case evalAt p ic of
                   Halt -> G.head ic
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
-- >>> evalAt 0 $ V.fromList [1, 3, 2, 1]
-- Mod {pos = 1, val = 3}
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
