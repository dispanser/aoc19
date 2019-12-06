{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Intcode where

import Data.Vector.Generic ((!), (//))
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace, traceShow)

-- | the state of the intcode interpreter: memory, instruction pointer, queue of provided inputs
data IState = IState { mem :: V.Vector Int
                     , ip  :: Int
                     , inputQueue :: [Int]} deriving (Eq, Show)

data Action  = Terminate | Output Int | Input Int | None deriving (Eq, Show)

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
        | IfTrue { cond :: Param
                 , jump :: Param }
        | Term
        deriving (Eq, Show)

initialize :: V.Vector Int -> IState
initialize mem = IState { mem = mem, ip = 0, inputQueue = [] }

withInputs :: IState -> [Int] -> IState
withInputs is inp = is { inputQueue = inp }

-- | run an @intcode@ program, return the list of actions
--
-- >>> runProgram (initialize $ V.fromList [1,11,12,3,2,3,13,0,4,0,99,30,40,50])
-- ([Output 3500,Terminate],3500)
-- >>> runProgram (initialize $ V.fromList [1,0,0,0,4,0,99])
-- ([Output 2,Terminate],2)
-- >>> runProgram (initialize $ V.fromList [1,1,1,4,99,5,6,0,4,0,99])
-- ([Output 30,Terminate],30)
runProgram :: IState -> ([Action], Int)
runProgram ic =
    let (actions, finalState) = go ic
    in (actions, V.head $ mem finalState)
 where go is = let (action, newState) = step is
                   (next, finalState) = go newState
               in case action of
                    Terminate -> ([Terminate], newState)
                    None      -> (next, finalState)
                    a         -> (a : next, finalState)

-- | advancing an intcode programm by one step.
--
-- >>> step $ initialize $ V.fromList [99]
-- (Terminate,IState {mem = [99], ip = 1, inputQueue = []})
-- >>> step $ initialize $ V.fromList [1, 3, 2, 1]
-- (None,IState {mem = [1,3,2,1], ip = 4, inputQueue = []})
-- >>> step $ initialize $ V.fromList [2, 3, 2, 1]
-- (None,IState {mem = [2,2,2,1], ip = 4, inputQueue = []})
-- >>> step $ initialize $ V.fromList [1,9,10,3,2,3,11,0,4,0,99,30,40,50]
-- (None,IState {mem = [1,9,10,99,2,3,11,0,4,0,99,30,40,50], ip = 4, inputQueue = []})
-- >>> step $ initialize $ V.fromList [4, 3, 2, 17]
-- (Output 17,IState {mem = [4,3,2,17], ip = 2, inputQueue = []})
-- >>> step $ initialize $ V.fromList [4, 3, 2, 17]
-- (Output 17,IState {mem = [4,3,2,17], ip = 2, inputQueue = []})
-- >>> step $ initialize (V.fromList [4, 3, 2, 17]) `withInputs` [1,2,3]
-- (Output 17,IState {mem = [4,3,2,17], ip = 2, inputQueue = [1,2,3]})
step :: IState -> (Action, IState)
step is@IState {..} =
    let (ip', instruction) = readOp is
    in case instruction of
         Term       -> (Terminate, is { ip = ip' })
         Add { .. } -> let newVal = evalParam is p1 + evalParam is p2
                           mem'   = mem // [(value out, newVal)]
                       in (None, is { ip = ip' , mem = mem'})
         Mul { .. } -> let newVal = evalParam is p1 * evalParam is p2
                           mem'   = mem // [(value out, newVal)]
                       in (None, is { ip = ip' , mem = mem'})
         Read { .. } -> let outVal = evalParam is p1
                        in (Output outVal, is { ip = ip' })
         Store { .. } -> let pos  = evalParam is p1
                             val  = head inputQueue
                             mem' = mem // [(pos, val)]
                         in (Input val, is { ip = ip'
                                           , mem = mem'
                                           , inputQueue = tail inputQueue })

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
-- >>> readOp $ initialize $ V.fromList [104, 17, 80, 1]
-- (2,Read {p1 = Param {mode = Direct, value = 17}})
-- >>> readOp $ initialize $ V.fromList [4, 917, 80, 1]
-- (2,Read {p1 = Param {mode = MemAddr, value = 917}})
readOp :: IState -> (Int, Op)
readOp is@IState { .. } =
    let (mode, opcode) = (mem ! ip) `divMod` 100
        param = readParam is mode
    in case opcode of
         99 -> (ip + 1, Term)
         1  -> (ip + 4, Add { p1 =  param 0 , p2 =  param 1 , out = param 2 })
         2  -> (ip + 4, Mul { p1 =  param 0 , p2 =  param 1 , out = param 2 })
         3  -> (ip + 2, Store $ Param Direct $ mem ! (ip + 1))
         4  -> (ip + 2, Read $ param 0)
         _  -> error $ "unsupported operation: " ++ show opcode

readParam :: IState  -> Int -> Int -> Param
readParam IState { .. } mode idx = Param (paramMode mode idx) $ mem ! (ip + idx + 1)

paramMode :: Int -> Int -> Mode
paramMode mode pos =
    let shiftRight = mode `div` 10 ^ pos
    in case shiftRight `mod` 10 of
         0 -> MemAddr
         1 -> Direct
         _ -> error $ "unsupported param mode " ++ show (shiftRight `mod` 10)

-- | result of an operation: change the value of a specific cell
data Mod = Mod { pos :: Int
               , val :: Int }
         | Halt deriving (Show, Eq)

debugShow :: Show a => String -> a -> a
debugShow prefix v =
    let msg = prefix ++ " " ++ show v
    in trace msg v

