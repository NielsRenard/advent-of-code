module Year2019.Day05 where

-- this puzzle continues from Day02

import Data.Function ((&))
import Data.List
import Data.List.Index
import Utils

-- The program will request an  ID of the system to test.
-- Provide it with 1, the ID for the ship's air conditioner unit.
solvePartOne :: Int
solvePartOne = getDiagnosticCode $ getOutput $ runProgramID 1

runProgramID id =
  slurp (provideInput id input) 2 []

provideInput programID xs = setAt (xs !! 1) programID xs

newtype Result = Result (Program, Output) deriving (Show)

slurp :: Program -> Index -> Output -> Result
slurp [] index acc = Result ([], acc)
slurp xs index acc =
  if index >= length xs
    then Result (xs, acc)
    else slurp xs' index' acc'
  where
    opCode = xs !! index
    firstArgPos = xs !! (xs !! (index + 1))
    secondArgPos = xs !! (xs !! (index + 2))
    thirdArg = xs !! (index + 3)
    firstArgImmediate = xs !! (index + 1)
    secondArgImmediate = xs !! (index + 2)
    --    thirdArgImmediate = xs !! (index + 3)
    args = case digits opCode of
      [1] -> firstArgPos + secondArgPos
      [2] -> firstArgPos * secondArgPos
      [4] -> firstArgPos
      [1, 0, 4] -> firstArgImmediate
      [1, 1, 0, 1] -> firstArgImmediate + secondArgImmediate
      [1, 0, 0, 1] -> firstArgPos + secondArgImmediate
      [1, 0, 1] -> firstArgImmediate + secondArgPos
      [1, 0, 2] -> firstArgImmediate * secondArgPos
      [1, 1, 0, 2] -> firstArgImmediate * secondArgImmediate
      [1, 0, 0, 2] -> firstArgPos * secondArgImmediate
      [99] -> 0
    index'
      | opCode `elem` [1, 2] = index + 4
      | opCode `elem` [1100, 1101, 1001, 1002, 1102, 102, 101] = index + 4
      | opCode `elem` [3, 4, 104] = index + 2
      | otherwise = length xs
    xs' = case digits opCode of
      [1] -> setAt thirdArg args xs
      [2] -> setAt thirdArg args xs
      [4] -> xs
      [9, 9] -> xs
      [1, 0, 1] -> setAt thirdArg args xs
      [1, 0, 2] -> setAt thirdArg args xs
      [1, 0, 4] -> xs
      [1, 1, 0, 1] -> setAt thirdArg args xs
      [1, 0, 0, 1] -> setAt thirdArg args xs
      [1, 0, 0, 2] -> setAt thirdArg args xs
      [1, 1, 0, 2] -> setAt thirdArg args xs
    acc'
      | opCode == 4 =
        acc ++ [xs !! (xs !! succ index)] -- position mode first arg
      | opCode == 104 =
        acc ++ [xs !! (xs !! (xs !! succ index))] -- immediate mode first arg
      | otherwise = acc

-- not 3761776537 too high (problems in test output)
-- yes 15259545

--bunch of types to make the intcode machine nicer to work with------------------
type Program = [Int]

type Output = [Int]

type Index = Int

getFinalProgram :: Result -> Output
getFinalProgram (Result (_, age)) = age

-- (...) it will run an output instruction indicating how far the result  was from the expected value
-- 0 means the test was successful. Non-zero outputs mean that a function is not working correctly;
-- If all outputs were zero except the diagnostic code, the diagnostic program ran successfully.
getOutput :: Result -> Output
getOutput (Result (_, age)) = age

-- FINALLY it will output a diagnostic code and immediately halt. An output followed immediately by a halt means the program finished.
getDiagnosticCode :: Output -> Int
getDiagnosticCode = last

--(example)input below----------------------------------------------------------
exampleInput :: [Int]
exampleInput =
  -- 0  1  2    3  4   5  6  7     8
  [2, 0, 102, 0, 7, 7, 4, 1101, 99]

-- this program  outputs whatever it gets as input, then halts.
inAndOut :: [Int]
inAndOut = [3, 0, 4, 0, 99]

-- * Year2019.Day05> slurp (provideInput 99 inAndOut ) 2 []

-- * Year2019.Day05> ([99,0,4,0,99],[99])

input :: [Int]
input = [3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0, 1102, 17, 65, 225, 102, 21, 95, 224, 1001, 224, -1869, 224, 4, 224, 1002, 223, 8, 223, 101, 7, 224, 224, 1, 224, 223, 223, 101, 43, 14, 224, 1001, 224, -108, 224, 4, 224, 102, 8, 223, 223, 101, 2, 224, 224, 1, 223, 224, 223, 1101, 57, 94, 225, 1101, 57, 67, 225, 1, 217, 66, 224, 101, -141, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 1, 224, 1, 224, 223, 223, 1102, 64, 34, 225, 1101, 89, 59, 225, 1102, 58, 94, 225, 1002, 125, 27, 224, 101, -2106, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 5, 224, 1, 224, 223, 223, 1102, 78, 65, 225, 1001, 91, 63, 224, 101, -127, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 3, 224, 1, 223, 224, 223, 1102, 7, 19, 224, 1001, 224, -133, 224, 4, 224, 102, 8, 223, 223, 101, 6, 224, 224, 1, 224, 223, 223, 2, 61, 100, 224, 101, -5358, 224, 224, 4, 224, 102, 8, 223, 223, 101, 3, 224, 224, 1, 224, 223, 223, 1101, 19, 55, 224, 101, -74, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 1, 224, 1, 224, 223, 223, 1101, 74, 68, 225, 4, 223, 99, 0, 0, 0, 677, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1105, 0, 99999, 1105, 227, 247, 1105, 1, 99999, 1005, 227, 99999, 1005, 0, 256, 1105, 1, 99999, 1106, 227, 99999, 1106, 0, 265, 1105, 1, 99999, 1006, 0, 99999, 1006, 227, 274, 1105, 1, 99999, 1105, 1, 280, 1105, 1, 99999, 1, 225, 225, 225, 1101, 294, 0, 0, 105, 1, 0, 1105, 1, 99999, 1106, 0, 300, 1105, 1, 99999, 1, 225, 225, 225, 1101, 314, 0, 0, 106, 0, 0, 1105, 1, 99999, 107, 677, 677, 224, 102, 2, 223, 223, 1006, 224, 329, 1001, 223, 1, 223, 1008, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 344, 1001, 223, 1, 223, 7, 226, 677, 224, 102, 2, 223, 223, 1005, 224, 359, 1001, 223, 1, 223, 8, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 374, 1001, 223, 1, 223, 1007, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 389, 101, 1, 223, 223, 8, 677, 226, 224, 1002, 223, 2, 223, 1005, 224, 404, 101, 1, 223, 223, 1108, 677, 226, 224, 102, 2, 223, 223, 1006, 224, 419, 1001, 223, 1, 223, 1108, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 434, 101, 1, 223, 223, 1108, 677, 677, 224, 1002, 223, 2, 223, 1005, 224, 449, 101, 1, 223, 223, 1008, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 464, 101, 1, 223, 223, 7, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 479, 101, 1, 223, 223, 108, 677, 677, 224, 1002, 223, 2, 223, 1005, 224, 494, 101, 1, 223, 223, 107, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 509, 101, 1, 223, 223, 107, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 524, 1001, 223, 1, 223, 1107, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 539, 101, 1, 223, 223, 1008, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 554, 1001, 223, 1, 223, 8, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 569, 101, 1, 223, 223, 1007, 677, 677, 224, 102, 2, 223, 223, 1005, 224, 584, 1001, 223, 1, 223, 1107, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 599, 101, 1, 223, 223, 7, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 614, 101, 1, 223, 223, 108, 677, 226, 224, 1002, 223, 2, 223, 1005, 224, 629, 1001, 223, 1, 223, 108, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 644, 101, 1, 223, 223, 1007, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 659, 101, 1, 223, 223, 1107, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 674, 1001, 223, 1, 223, 4, 223, 99, 226]
