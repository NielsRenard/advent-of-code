{-# HLINT ignore "Eta reduce" #-}

module Year2019.Day05 where
-- this puzzle continues from Day02
import Data.List
import Data.List.Index
import Utils
import Data.Function ((&))

-- (...) it will run an output instruction indicating how far the result  was from the expected value
-- 0 means the test was successful. Non-zero outputs mean that a function is not working correctly;
-- FINALLY it will output a diagnostic code and immediately halt. An output followed immediately by a halt means the program finished.
-- If all outputs were zero except the diagnostic code, the diagnostic program ran successfully.


-- The program will request an  ID of the system to test.
-- Provide it with 1, the ID for the ship's air conditioner unit.
readyInput = provideInput 1 input

exampleInput :: [Int]
exampleInput =
  [2, 0, 102,    0, 7,  7,   4, 1101, 99]
-- 0  1   2    3   4   5   6  7  ,  8
-- The program  outputs whatever it gets as input, then halts.
inAndOut :: [Int]
inAndOut = [3,0,4,0,99]
-- *Year2019.Day05> slurp (provideInput 99 inAndOut ) 2 []
-- ([99,0,4,0,99],[99])


-- this function does all the work (based on Day02)
slurp :: [Int] -> Int -> [Int]-> ([Int], [Int])
slurp [] index acc = ([], acc)
slurp xs index acc =
  if index >= length xs -- maybe eq? (or opCode outOfBounds)
    then (xs, acc)
    else slurp xs' index' acc'
  where
    opCode = xs !! index
    firstArgPos = xs !! (xs !! (index + 1))
    secondArgPos = xs !! (xs !! (index + 2))
    thirdArg = xs !! (index + 3)
--    thirdArgImmediate = xs !! (index + 3)    
    firstArgImmediate = xs !! (index + 1)
    secondArgImmediate = xs !! (index + 2)
    args = case digits opCode of
      [1] -> firstArgPos + secondArgPos
      [2] -> firstArgPos * secondArgPos
      [4] -> firstArgPos
      [1,0,4] -> firstArgImmediate      
--      [0,0,0,1] -> firstArgPos + secondArgPos
      [1,1,0,1] -> firstArgImmediate + secondArgImmediate
      [1,0,0,1] -> firstArgPos + secondArgImmediate      
--      [0,0,0,2] -> firstArgPos * secondArgPos
      [1,0,1] -> firstArgImmediate + secondArgPos
      [1,0,2] -> firstArgImmediate * secondArgPos
      [1,1,0,2] -> firstArgImmediate * secondArgImmediate
      [1,0,0,2] -> firstArgImmediate * secondArgImmediate      
      [99] -> 0
    index' | opCode `elem` [1,2] = index + 4
           | opCode `elem` [1100,1101,1001,1002,1102,102,101] = index + 4    
           | opCode `elem` [3,4,104] = index + 2
--           | opCode > 999 && opCode < 10000 = index + 4
           | otherwise = length xs
    xs' = case digits opCode of
--      [1,_,_,_,_] -> setAt thirdArgImmediate args xs
--      [0,_,_,_,_] -> setAt thirdArgImmediate args xs
      [1] -> setAt thirdArg args xs
      [2] -> setAt thirdArg args xs
      [4] -> xs
      [9,9] -> xs      
      [1,0,1] -> setAt thirdArg args xs
      [1,0,2] -> setAt thirdArg args xs
      [1,0,4] -> xs
      [1,1,0,1] -> setAt thirdArg args xs
      [1,0,0,1] -> setAt thirdArg args xs
      [1,0,0,2] -> setAt thirdArg args xs            
      [1,1,0,2] -> setAt thirdArg args xs
      --      [0,0,0,1] -> setAt thirdArg args xs
--      [0,0,0,2] -> setAt thirdArg args xs

--    xs'  | opCode == 1 = setAt thirdArg args xs
--         | opCode == 2 = setAt thirdArg args xs

--         | length (digits opCode) 
--         | opCode > 999 && opCode < 10000  = xs -- catches everything, now rebuild the whole thing to support "mode parameters"
    acc' | opCode == 4 =
             acc ++ [xs !! (xs !! succ index)]  -- position mode first arg
         | opCode == 104 =
             acc ++ [xs !! (xs !! (xs !! succ index))] -- immediate mode
         | otherwise = acc
         
-- not 3761776537 too high
provideInput programID xs = setAt (xs !! 1) programID xs

insertNounAndVerb n v = setAt 1 n . setAt 2 v

plusInstr (n1, n2, index) xs = setAt index (xs !! n1 + xs !! n2) xs

multInstr (n1, n2, index) xs = setAt index (xs !! n1 * xs !! n2) xs

inputInstr (index, n1) xs = setAt index n1 xs

outputInstr (index, n1) xs = setAt index n1 xs







--input below-------------------------------------------------------------------
input :: [Int]
input=[3,225,1,225,6,6,1100,1,238,225,104,0,1102,17,65,225,102,21,95,224,1001,224,-1869,224,4,224,1002,223,8,223,101,7,224,224,1,224,223,223,101,43,14,224,1001,224,-108,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1101,57,94,225,1101,57,67,225,1,217,66,224,101,-141,224,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,1102,64,34,225,1101,89,59,225,1102,58,94,225,1002,125,27,224,101,-2106,224,224,4,224,102,8,223,223,1001,224,5,224,1,224,223,223,1102,78,65,225,1001,91,63,224,101,-127,224,224,4,224,102,8,223,223,1001,224,3,224,1,223,224,223,1102,7,19,224,1001,224,-133,224,4,224,102,8,223,223,101,6,224,224,1,224,223,223,2,61,100,224,101,-5358,224,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,19,55,224,101,-74,224,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,1101,74,68,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,107,677,677,224,102,2,223,223,1006,224,329,1001,223,1,223,1008,226,677,224,102,2,223,223,1006,224,344,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,359,1001,223,1,223,8,226,226,224,102,2,223,223,1006,224,374,1001,223,1,223,1007,226,226,224,102,2,223,223,1006,224,389,101,1,223,223,8,677,226,224,1002,223,2,223,1005,224,404,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,419,1001,223,1,223,1108,226,677,224,102,2,223,223,1006,224,434,101,1,223,223,1108,677,677,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,464,101,1,223,223,7,677,226,224,1002,223,2,223,1006,224,479,101,1,223,223,108,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,107,226,677,224,1002,223,2,223,1006,224,509,101,1,223,223,107,226,226,224,102,2,223,223,1006,224,524,1001,223,1,223,1107,226,677,224,1002,223,2,223,1006,224,539,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,554,1001,223,1,223,8,226,677,224,1002,223,2,223,1006,224,569,101,1,223,223,1007,677,677,224,102,2,223,223,1005,224,584,1001,223,1,223,1107,677,226,224,1002,223,2,223,1006,224,599,101,1,223,223,7,226,226,224,1002,223,2,223,1005,224,614,101,1,223,223,108,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,108,226,226,224,1002,223,2,223,1005,224,644,101,1,223,223,1007,677,226,224,1002,223,2,223,1006,224,659,101,1,223,223,1107,226,226,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226]

inputDayTwo :: [Int]
inputDayTwo = [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 10, 19, 1, 6, 19, 23, 1, 10, 23, 27, 2, 27, 13, 31, 1, 31, 6, 35, 2, 6, 35, 39, 1, 39, 5, 43, 1, 6, 43, 47, 2, 6, 47, 51, 1, 51, 5, 55, 2, 55, 9, 59, 1, 6, 59, 63, 1, 9, 63, 67, 1, 67, 10, 71, 2, 9, 71, 75, 1, 6, 75, 79, 1, 5, 79, 83, 2, 83, 10, 87, 1, 87, 5, 91, 1, 91, 9, 95, 1, 6, 95, 99, 2, 99, 10, 103, 1, 103, 5, 107, 2, 107, 6, 111, 1, 111, 5, 115, 1, 9, 115, 119, 2, 119, 10, 123, 1, 6, 123, 127, 2, 13, 127, 131, 1, 131, 6, 135, 1, 135, 10, 139, 1, 13, 139, 143, 1, 143, 13, 147, 1, 5, 147, 151, 1, 151, 2, 155, 1, 155, 5, 0, 99, 2, 0, 14, 0]
