{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Intcode where

import Debug.Trace (trace)

import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Data.IntMap ((!), (!?))
import qualified Data.IntMap as IM


type Program = IState -> ([Action], IState)

type IntMemory = IM.IntMap Int

-- | the state of the intcode interpreter: memory, instruction pointer, queue of provided inputs,
-- and current data pointer (relativeBase)
data IState = IState { mem :: IntMemory
                     , ip  :: Int
                     , inputQueue :: [Int]
                     , relativeBase :: Int } deriving (Eq, Show)

data Action  = Terminate | Output Int | Input Int | InputRequired | None deriving (Eq, Show)

-- | interpret directly as a value or as a memory address
data Mode  = Direct | MemAddr | Relative deriving (Show, Eq)

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
        | IfFalse { cond :: Param
                  , jump :: Param }
        | LessThan { p1 :: Param
                   , p2 :: Param
                   , out :: Param }
        | Equals { p1 :: Param
                 , p2 :: Param
                 , out :: Param }
        | Term
        | AdjustBase { diff :: Param }
        deriving (Eq, Show)

initialize :: [Int] -> IState
initialize mem = IState { mem = IM.fromList $ zip [0..] mem, ip = 0, inputQueue = [], relativeBase = 0 }

updateCell :: IntMemory -> Int -> Int -> IntMemory
updateCell mem cell val = trace' ("[" ++ show cell ++ "]<-" ++ show val) $ IM.insert cell val mem

readCell :: IState -> Int -> Int
readCell is cell =
    let val = fromMaybe 0 $ mem is !? cell
    -- in trace' (show cell ++ ": " ++ show val) val
    in val

withInputs :: IState -> [Int] -> IState
withInputs is inp = is { inputQueue = inp }

-- | run an @intcode@ program, return the list of actions
--
-- >>> runProgram (initialize [1,11,12,3,2,3,13,0,4,0,99,30,40,50])
-- ([Output 3500,Terminate],3500)
-- >>> runProgram (initialize [1,0,0,0,4,0,99])
-- ([Output 2,Terminate],2)
-- >>> runProgram (initialize [1,1,1,4,99,5,6,0,4,0,99])
-- ([Output 30,Terminate],30)
-- >>> runProgram $ (initialize [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) `withInputs` [0]
-- ([Input 0,Output 0,Terminate],3)
-- >>> runProgram $ (initialize $ [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) `withInputs` [1]
-- ([Input 1,Output 1,Terminate],3)
-- >>> runProgram $ (initialize $ [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) `withInputs` [0]
-- ([Input 0,Output 0,Terminate],3)
-- >>> runProgram $ (initialize $ [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]) `withInputs` [1]
-- ([Input 1,Output 1,Terminate],3)
-- >>> let input = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
-- >>> fst $ runProgram $ (initialize input) `withInputs` [-13]
-- [Input (-13),Output 999,Terminate]
-- >>> fst $ runProgram $ (initialize input) `withInputs` [8]
-- [Input 8,Output 1000,Terminate]
-- >>> fst $ runProgram $ (initialize input) `withInputs` [111]
-- [Input 111,Output 1001,Terminate]
-- >>> runProgram (initialize $ [3, 2, 0])
-- ([InputRequired],3)
-- >>> getOutputs . fst . runProgram $ initialize [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
-- [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
-- >>> head . getOutputs . fst . runProgram $ initialize [1102,34915192,34915192,7,4,7,99,0]
-- 1219070632396864
-- >>> head . getOutputs . fst . runProgram $ initialize [104,1125899906842624,99]
-- 1125899906842624
runProgram :: IState -> ([Action], Int)
runProgram ic =
    let (actions, finalState) = runProgram' ic
    in (actions, readCell finalState 0)

runProgram' :: IState -> ([Action], IState)
runProgram' ic = let (action, newState) = step ic
                     (next, finalState) = runProgram' newState
               in case action of
                    Terminate     -> ([Terminate], newState)
                    InputRequired -> ([InputRequired], newState)
                    None      -> (next, finalState)
                    a         -> (a : next, finalState)

-- | connect two programs by wiring the output of the first program to the input
-- of the second one. The execution will be pull-based, i.e. the second program
-- will run as long as it can, and the first program is only started to provide
-- further inputs to the other program.
composePrograms :: IState -> IState -> IState
composePrograms p1 p2 =
    -- we're ignoring newState here, a clear sign that whatever we do is not useful
    let (actions, newState) = runProgram' p1
    in p2 `withInputs` (inputQueue p2 ++ getOutputs actions)

getOutputs :: [Action] -> [Int]
getOutputs = mapMaybe f
 where f = \case
             Output n -> Just n
             _        -> Nothing

readMem :: IState -> [Int]
readMem = IM.elems . mem

-- | advancing an intcode programm by one step.
--
-- >>> fst . step $ initialize $ [99]
-- Terminate
-- >>> readMem . snd . step $ initialize $ [1, 3, 2, 1]
-- [1,3,2,1]
-- >>> readMem . snd . step $ initialize $ [2, 3, 2, 1]
-- [2,2,2,1]
-- >>> let is = snd . step $ initialize $ [1,9,10,3,2,3,11,0,4,0,99,30,40,50]
-- >>> readMem is
-- [1,9,10,99,2,3,11,0,4,0,99,30,40,50]
-- >>> ip is
-- 4
-- >>> fst . step $ initialize $ [4, 3, 2, 17]
-- Output 17
-- >>> ip . snd . step $ initialize $ [1105, 1, 17]
-- 17
-- >>> ip . snd . step $ initialize $ [1105, 0, 17]
-- 3
-- >>> ip . snd . step $ initialize $ [5, 0, 1]
-- 0
-- >>> ip . snd . step $ initialize $ [6, 0, 1]
-- 3
-- >>> readMem . snd . step $ initialize $ [1107, 0, 1, 3]
-- [1107,0,1,1]
-- >>> readMem . snd . step $ initialize $ [1107, 2, 1, 3]
-- [1107,2,1,0]
-- >>> readMem . snd . step $ initialize $ [1107, 1, 1, 3]
-- [1107,1,1,0]
-- >>> readMem . snd . step $ initialize $ [7, 2, 3, 4, 1]
-- [7,2,3,4,1]
-- >>> let res = step $ (initialize $ [3, 2, 0]) `withInputs` [7]
-- >>> fst res
-- Input 7
-- >>> readMem $ snd res
-- [3,2,7]
-- >>> inputQueue $ snd res
-- []
-- >>> fst . step $ (initialize $ [3, 2, 0])
-- InputRequired
-- >>> relativeBase . snd . step $ (initialize $ [109, 19, 0])
-- 19
-- >>> relativeBase . snd . step $ (initialize $ [109, 19, 0]) { relativeBase = 17}
-- 36
-- >>> fst . step $ (initialize $ [204, -2, 3, 0, 5]) { relativeBase = 2}
-- Output 204
step :: IState -> (Action, IState)
step is@IState {..} =
    let (ip', instruction) = readOp is
    in case (debugShow "instruction" instruction) of
        Term       -> (Terminate, is { ip = ip' })
        Add { .. } -> let newVal = evalParam is p1 + evalParam is p2
                          mem'   = updateCell mem (evalOutParam is out) newVal
                      in (None, is { ip = ip' , mem = mem'})
        Mul { .. } -> let newVal = evalParam is p1 * evalParam is p2
                          mem'   = updateCell mem (evalOutParam is out) newVal
                      in (None, is { ip = ip' , mem = mem'})
        Store { .. } -> let pos  = evalOutParam is p1
                        in case listToMaybe inputQueue of
                             Just val -> (Input val, is
                                 { ip = ip'
                                 , mem = updateCell mem pos val
                                 , inputQueue = tail inputQueue })
                             Nothing -> (InputRequired, is)
        Read { .. } -> let outVal = evalParam is p1
                       in (Output outVal, is { ip = ip' })
        IfTrue {..} -> if evalParam is cond /= 0
                          then (None, is { ip = evalParam is jump })
                          else (None, is { ip = ip' })
        IfFalse {..} -> if evalParam is cond == 0
                           then (None, is { ip = evalParam is jump })
                           else (None, is { ip = ip' })
        LessThan {..} -> let newVal  = if evalParam is p1 < evalParam is p2 then 1 else 0
                         in (None, is { ip = ip', mem = updateCell mem (evalOutParam is out) newVal } )
        Equals {..} ->  let newVal  = if evalParam is p1 == evalParam is p2 then 1 else 0
                        in (None, is { ip = ip', mem = updateCell mem (evalOutParam is out) newVal } )
        AdjustBase {..} -> (None, is { ip = ip', relativeBase = relativeBase + evalParam is diff })


-- | read a parameter from a program
--
-- >>> evalParam (initialize $ [1, 3, 2, 1]) $ Param MemAddr 3
-- 1
-- >>> evalParam (initialize $ [1, 3, 2, 1]) $ Param Direct 3
-- 3
evalParam :: IState -> Param -> Int
evalParam is (Param MemAddr value)  = debugShow ("@[" ++ show value ++ "]==") $ readCell is value
evalParam _  (Param Direct value )  = value
evalParam is (Param Relative value) = debugShow ("@[" ++ show value ++ "+" ++ show (relativeBase is) ++ "]==") $ readCell is (value + relativeBase is)

evalOutParam :: IState -> Param -> Int
evalOutParam _  (Param MemAddr v)  = v
evalOutParam _  (Param Direct v)   = v
evalOutParam is (Param Relative v) = relativeBase is + v


peekOp :: IState -> String
peekOp is = show (readCell is . (+ ip is) <$> [0..3]) ++ " rB=" ++ show (relativeBase is)

-- | read operation from a machine state
--
-- >>> readOp $ initialize $ [99]
-- (1,Term)
-- >>> readOp $ initialize $ [1, 3, 2, 1]
-- (4,Add {p1 = Param {mode = MemAddr, value = 3}, p2 = Param {mode = MemAddr, value = 2}, out = Param {mode = MemAddr, value = 1}})
-- >>> readOp $ initialize $ [2, 3, 2, 1]
-- (4,Mul {p1 = Param {mode = MemAddr, value = 3}, p2 = Param {mode = MemAddr, value = 2}, out = Param {mode = MemAddr, value = 1}})
-- >>> readOp $ initialize $ [1002, 3, 80, 1]
-- (4,Mul {p1 = Param {mode = MemAddr, value = 3}, p2 = Param {mode = Direct, value = 80}, out = Param {mode = MemAddr, value = 1}})
-- >>> readOp $ initialize $ [104, 17, 80, 1]
-- (2,Read {p1 = Param {mode = Direct, value = 17}})
-- >>> readOp $ initialize $ [4, 917, 80, 1]
-- (2,Read {p1 = Param {mode = MemAddr, value = 917}})
-- >>> readOp $ initialize $ [1105, 1, 17]
-- (3,IfTrue {cond = Param {mode = Direct, value = 1}, jump = Param {mode = Direct, value = 17}})
-- >>> readOp $ initialize $ [5, 0, 17]
-- (3,IfTrue {cond = Param {mode = MemAddr, value = 0}, jump = Param {mode = MemAddr, value = 17}})
-- >>> readOp $ initialize $ [6, 0, 17]
-- (3,IfFalse {cond = Param {mode = MemAddr, value = 0}, jump = Param {mode = MemAddr, value = 17}})
-- >>> readOp $ initialize $ [7, 0, 17, 19]
-- (4,LessThan {p1 = Param {mode = MemAddr, value = 0}, p2 = Param {mode = MemAddr, value = 17}, out = Param {mode = MemAddr, value = 19}})
-- >>> readOp $ initialize $ [8, 0, 17, 19]
-- (4,Equals {p1 = Param {mode = MemAddr, value = 0}, p2 = Param {mode = MemAddr, value = 17}, out = Param {mode = MemAddr, value = 19}})
readOp :: IState -> (Int, Op)
readOp is@IState { .. } =
    let (mode, opcode) = (mem ! ip) `divMod` 100
        param = readParam (debugShowF peekOp "peek" is) mode
    in case opcode of
         99 -> (ip + 1, Term)
         1  -> (ip + 4, Add { p1 =  param 0 , p2 =  param 1 , out = param 2 })
         2  -> (ip + 4, Mul { p1 =  param 0 , p2 =  param 1 , out = param 2 })
         3  -> (ip + 2, Store $ param 0)
         4  -> (ip + 2, Read $ param 0)
         5  -> (ip + 3, IfTrue (param 0) (param 1))
         6  -> (ip + 3, IfFalse (param 0) (param 1))
         7  -> (ip + 4, LessThan (param 0) (param 1) (param 2))
         8  -> (ip + 4, Equals (param 0) (param 1) (param 2))
         9  -> (ip + 2, AdjustBase $ param 0)
         _  -> error $ "unsupported opcode: " ++ show opcode

readParam :: IState -> Int -> Int -> Param
readParam IState { .. } mode idx = Param (paramMode mode idx) $ mem ! (ip + idx + 1)

paramMode :: Int -> Int -> Mode
paramMode mode pos =
    let shiftRight = mode `div` 10 ^ pos
    in case shiftRight `mod` 10 of
         0 -> MemAddr
         1 -> Direct
         2 -> Relative
         _ -> error $ "unsupported param mode " ++ show (shiftRight `mod` 10)

-- | result of an operation: change the value of a specific cell
data Mod = Mod { pos :: Int
               , val :: Int }
         | Halt deriving (Show, Eq)

debugShowF :: Show b => (a -> b) -> String -> a -> a
debugShowF f prefix v =
    let msg = prefix ++ " " ++ show (f v)
    in trace' msg v

debugShow :: Show a => String -> a -> a
debugShow = debugShowF id

trace' :: String -> a -> a
-- trace' = trace
trace' _ b = b
