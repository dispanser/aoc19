module Day7 where

import Data.List (permutations)
import qualified Data.Vector.Unboxed as V
import qualified Intcode as I
import Debug.Trace (trace)


-- |
--
-- >>> part1 $ [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
-- 43210
-- >>> part1 $ [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
-- 43210
-- >>> part1 $ [3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0]
-- 54321
-- >>> part1 $ [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
-- 65210
part1 :: [Int] -> Int
part1 vs =
    let program = I.initialize vs
        results = evaluate 0 program <$> permutations [0..4]
    in maximum results

part2 :: V.Vector Int -> Int
part2 vs = undefined

evaluate' :: Int -> I.IState -> [Int] -> Int
evaluate' input ic phases =
    let amps = I.withInputs ic . (:[]) <$> phases
    in undefined

-- | evaluate intcode program on the amplifiers with the provided phase settings
--
-- >>> evaluate 0 (I.initialize [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) [4,3,2,1,0]
-- 43210
-- >>> evaluate 0 (I.initialize [3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0]) [0,1,2,3,4]
-- 54321
-- >>> evaluate 0 (I.initialize [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) [1,0,4,3,2]
-- 65210
evaluate :: Int -> I.IState -> [Int] -> Int
evaluate prev _  []     = prev
evaluate prev ic (x:xs) =
    let actions       = fst . I.runProgram $ ic `I.withInputs` [x, prev]
        (I.Output next) = head $ filter isOutput actions
    in evaluate next ic xs

isOutput :: I.Action -> Bool
isOutput (I.Output _) = True
isOutput _          = False

debugShow :: Show a => String -> a -> a
debugShow prefix v =
    let msg = prefix ++ " " ++ show v
    in trace msg v
