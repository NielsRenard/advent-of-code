module Year2019.Intcode where

import Data.Function ((&))
--import Data.Set as Set hiding (take, drop)
import Data.List
import Data.List.Index
import Data.Tuple.Utils
import Utils

type Program = [Int]

type Output = [Int]

type Index = Int

type Phase = Int

type InputSignal = Int

getFinalProgram :: Result -> Output
getFinalProgram (Result (program, _)) = program

-- 0 means the test was successful. Non-zero outputs mean that a function is not working correctly;
-- If all outputs were zero except the diagnostic code, the diagnostic program ran successfully.
getOutput :: Result -> Output
getOutput (Result (_, output)) = output

-- FINALLY it will output a diagnostic code and immediately halt. An output followed immediately by a halt means the program finished.
getDiagnosticCode :: Result -> Int
getDiagnosticCode = last . getOutput

newtype Result = Result (Program, Output) deriving (Show)

intcodeUntilHalt :: [Int] -> Int -> [Int] -> [Int] -> ([Int], Int, [Int])
intcodeUntilHalt program@[] index inputs output = ([], 0, output) -- what is the instructionPointer here
intcodeUntilHalt program index inputs output =
  if index >= length program
    then (program, 99999, output)
    else intcodeUntilHalt program' index' inputs' output'
  where
    opCode = program !! index
    inputs' = case digits opCode of
      [3] -> drop 1 inputs
      _ -> inputs
    firstArgPos = program !! (program !! (index + 1))
    firstArgImmediate = program !! (index + 1)
    secondArgPos = program !! (program !! (index + 2))
    secondArgImmediate = program !! (index + 2)
    thirdArg = program !! (index + 3)
    args = case digits opCode of
      [1] -> firstArgPos + secondArgPos
      [2] -> firstArgPos * secondArgPos
      [3] -> head inputs
      -- support modes for 1103 etc?
      [4] -> firstArgPos
      [1, 0, 4] -> firstArgImmediate
      -- if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter.
      -- Otherwise, it does nothing.
      [5] -> if firstArgPos /= 0 then secondArgPos else index + 3
      [1, 0, 5] -> if firstArgImmediate /= 0 then secondArgPos else index + 3
      [1, 0, 0, 5] -> if firstArgPos /= 0 then secondArgImmediate else index + 3
      [1, 1, 0, 5] -> if firstArgImmediate /= 0 then secondArgImmediate else index + 3
      -- if the first parameter is zero, it sets the instruction pointer to the value from the second parameter.
      -- Otherwise, it does nothing.
      [6] -> if firstArgPos == 0 then secondArgPos else index + 3
      [1, 0, 6] -> if firstArgImmediate == 0 then secondArgPos else index + 3
      [1, 0, 0, 6] -> if firstArgPos == 0 then secondArgImmediate else index + 3
      [1, 1, 0, 6] -> if firstArgImmediate == 0 then secondArgImmediate else index + 3
      -- if the first parameter is less than the second parameter,
      -- it stores 1 in the position given by the third parameter.--  Otherwise, it stores 0.
      [7] -> if firstArgPos < secondArgPos then 1 else 0
      [1, 0, 7] -> if firstArgImmediate < secondArgPos then 1 else 0
      [1, 0, 0, 7] -> if firstArgPos < secondArgImmediate then 1 else 0
      [1, 1, 0, 7] -> if firstArgImmediate < secondArgImmediate then 1 else 0
      -- if the first parameter is equal to the second parameter,
      -- it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      [8] -> if firstArgPos == secondArgPos then 1 else 0
      [1, 0, 8] -> if firstArgImmediate == secondArgPos then 1 else 0
      [1, 0, 0, 8] -> if firstArgPos == secondArgImmediate then 1 else 0
      [1, 1, 0, 8] -> if firstArgImmediate == secondArgImmediate then 1 else 0
      [1, 1, 0, 1] -> firstArgImmediate + secondArgImmediate
      [1, 0, 0, 1] -> firstArgPos + secondArgImmediate
      [1, 0, 1] -> firstArgImmediate + secondArgPos
      [1, 0, 2] -> firstArgImmediate * secondArgPos
      [1, 1, 0, 2] -> firstArgImmediate * secondArgImmediate
      [1, 0, 0, 2] -> firstArgPos * secondArgImmediate
    --      [99] -> 0
    index'
      | opCode `elem` [1, 2, 7, 8] = index + 4 --   could combine with next line
      | opCode `elem` [1100, 1101, 1001, 1002, 1102, 102, 101, 107, 108, 1107, 1108, 1007, 1008] = index + 4
      | opCode `elem` [3, 4, 104] = index + 2
      -- jump-if-true
      | opCode `elem` [5, 105, 1005, 1105] = args
      -- jump-if-false
      | opCode `elem` [6, 106, 1006, 1106] = args
      | opCode == 99 = length program
    program' = case digits opCode of
      [1] -> setAt thirdArg args program
      [1, 0, 1] -> setAt thirdArg args program
      [1, 1, 0, 1] -> setAt thirdArg args program
      [1, 0, 0, 1] -> setAt thirdArg args program
      [2] -> setAt thirdArg args program
      [1, 0, 2] -> setAt thirdArg args program
      [1, 0, 0, 2] -> setAt thirdArg args program
      [1, 1, 0, 2] -> setAt thirdArg args program
      [3] -> setAt firstArgImmediate args program
      [4] -> program
      [1, 0, 4] -> program
      [5] -> program
      [1, 0, 5] -> program
      [1, 0, 0, 5] -> program
      [1, 1, 0, 5] -> program
      [6] -> program
      [1, 0, 6] -> program
      [1, 0, 0, 6] -> program
      [1, 1, 0, 6] -> program
      [7] -> setAt thirdArg args program
      [1, 0, 7] -> setAt thirdArg args program
      [1, 0, 0, 7] -> setAt thirdArg args program
      [1, 1, 0, 7] -> setAt thirdArg args program
      [8] -> setAt thirdArg args program
      [1, 0, 8] -> setAt thirdArg args program
      [1, 0, 0, 8] -> setAt thirdArg args program
      [1, 1, 0, 8] -> setAt thirdArg args program
      [9, 9] -> program
    --      _ -> digits opCode ++ [999]
    output'
      | opCode == 4 =
        output ++ [program !! (program !! succ index)] -- position mode first arg
      | opCode == 104 =
        output ++ [program !! (program !! (program !! succ index))] -- immediate mode first arg
      | otherwise = output

intcode :: [Int] -> Int -> [Int] -> [Int] -> ([Int], Int, [Int])
intcode program@[] index inputs output = ([], 0, output) -- what is the instructionPointer here
intcode program index inputs output =
  if index >= length program
    then (program, 99999, output)
    else
      if opCode == 4
        then (program', index', output')
        else intcode program' index' inputs' output'
  where
    opCode = program !! index
    inputs' = case digits opCode of
      [3] -> drop 1 inputs
      _ -> inputs
    firstArgPos = program !! (program !! (index + 1))
    firstArgImmediate = program !! (index + 1)
    secondArgPos = program !! (program !! (index + 2))
    secondArgImmediate = program !! (index + 2)
    thirdArg = program !! (index + 3)
    args = case digits opCode of
      [1] -> firstArgPos + secondArgPos
      [2] -> firstArgPos * secondArgPos
      [3] -> head inputs
      -- support modes for 1103 etc?
      [4] -> firstArgPos
      [1, 0, 4] -> firstArgImmediate
      -- if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter.
      -- Otherwise, it does nothing.
      [5] -> if firstArgPos /= 0 then secondArgPos else index + 3
      [1, 0, 5] -> if firstArgImmediate /= 0 then secondArgPos else index + 3
      [1, 0, 0, 5] -> if firstArgPos /= 0 then secondArgImmediate else index + 3
      [1, 1, 0, 5] -> if firstArgImmediate /= 0 then secondArgImmediate else index + 3
      -- if the first parameter is zero, it sets the instruction pointer to the value from the second parameter.
      -- Otherwise, it does nothing.
      [6] -> if firstArgPos == 0 then secondArgPos else index + 3
      [1, 0, 6] -> if firstArgImmediate == 0 then secondArgPos else index + 3
      [1, 0, 0, 6] -> if firstArgPos == 0 then secondArgImmediate else index + 3
      [1, 1, 0, 6] -> if firstArgImmediate == 0 then secondArgImmediate else index + 3
      -- if the first parameter is less than the second parameter,
      -- it stores 1 in the position given by the third parameter.--  Otherwise, it stores 0.
      [7] -> if firstArgPos < secondArgPos then 1 else 0
      [1, 0, 7] -> if firstArgImmediate < secondArgPos then 1 else 0
      [1, 0, 0, 7] -> if firstArgPos < secondArgImmediate then 1 else 0
      [1, 1, 0, 7] -> if firstArgImmediate < secondArgImmediate then 1 else 0
      -- if the first parameter is equal to the second parameter,
      -- it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
      [8] -> if firstArgPos == secondArgPos then 1 else 0
      [1, 0, 8] -> if firstArgImmediate == secondArgPos then 1 else 0
      [1, 0, 0, 8] -> if firstArgPos == secondArgImmediate then 1 else 0
      [1, 1, 0, 8] -> if firstArgImmediate == secondArgImmediate then 1 else 0
      [1, 1, 0, 1] -> firstArgImmediate + secondArgImmediate
      [1, 0, 0, 1] -> firstArgPos + secondArgImmediate
      [1, 0, 1] -> firstArgImmediate + secondArgPos
      [1, 0, 2] -> firstArgImmediate * secondArgPos
      [1, 1, 0, 2] -> firstArgImmediate * secondArgImmediate
      [1, 0, 0, 2] -> firstArgPos * secondArgImmediate
    --      [99] -> 0
    index'
      | opCode `elem` [1, 2, 7, 8] = index + 4 --   could combine with next line
      | opCode `elem` [1100, 1101, 1001, 1002, 1102, 102, 101, 107, 108, 1107, 1108, 1007, 1008] = index + 4
      | opCode `elem` [3, 4, 104] = index + 2
      -- jump-if-true
      | opCode `elem` [5, 105, 1005, 1105] = args
      -- jump-if-false
      | opCode `elem` [6, 106, 1006, 1106] = args
      | opCode == 99 = length program
    program' = case digits opCode of
      [1] -> setAt thirdArg args program
      [1, 0, 1] -> setAt thirdArg args program
      [1, 1, 0, 1] -> setAt thirdArg args program
      [1, 0, 0, 1] -> setAt thirdArg args program
      [2] -> setAt thirdArg args program
      [1, 0, 2] -> setAt thirdArg args program
      [1, 0, 0, 2] -> setAt thirdArg args program
      [1, 1, 0, 2] -> setAt thirdArg args program
      [3] -> setAt firstArgImmediate args program
      [4] -> program
      [1, 0, 4] -> program
      [5] -> program
      [1, 0, 5] -> program
      [1, 0, 0, 5] -> program
      [1, 1, 0, 5] -> program
      [6] -> program
      [1, 0, 6] -> program
      [1, 0, 0, 6] -> program
      [1, 1, 0, 6] -> program
      [7] -> setAt thirdArg args program
      [1, 0, 7] -> setAt thirdArg args program
      [1, 0, 0, 7] -> setAt thirdArg args program
      [1, 1, 0, 7] -> setAt thirdArg args program
      [8] -> setAt thirdArg args program
      [1, 0, 8] -> setAt thirdArg args program
      [1, 0, 0, 8] -> setAt thirdArg args program
      [1, 1, 0, 8] -> setAt thirdArg args program
      [9, 9] -> program
    --      _ -> digits opCode ++ [999]
    output'
      | opCode == 4 =
        output ++ [program !! (program !! succ index)] -- position mode first arg
      | opCode == 104 =
        output ++ [program !! (program !! (program !! succ index))] -- immediate mode first arg
      | otherwise = output
