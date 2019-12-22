module Year2019.Day07 where

-- this puzzle continues from Day05

import Data.Function ((&))
import Data.Set as Set hiding (take)
import Data.List
import Data.Tuple.Utils
import Data.List.Index
import Utils
import qualified Year2019.Intcode as IC

--solvePartOne :: Program -> Output
solvePartOne input = maximum $ [getOutput $ amplify [a,b,c,d,e] 0 input 0 | 
                 a <- [0..4],
                 b <- [0..4],
                 c <- [0..4],
                 d <- [0..4],
                 e <- [0..4],
                 (Set.fromList [a,b,c,d,e] & Set.size) == 5]

--[getOutput $ amplify [a,b,c,d,e] 0 input 0 | a <- [0..4], b <- [0..4], c <- [0..4], d <- [0..4], e <- [0..4], (Set.fromList [a,b,c,d,e] & Set.size) == 5]

--solvePartTwo :: Program -> [Output]
solvePartTwo input = [feedback [a,b,c,d,e] 0 input 0 [0,0,0,0,0] False |
                 a <- [5..9],
                 b <- [5..9],
                 c <- [5..9],
                 d <- [5..9],
                 e <- [5..9],
                 (Set.fromList [a,b,c,d,e] & Set.size) == 5]

amplify :: [Phase] -> Int -> Program -> Int -> ([Int],Int,[Int])
amplify phases inputSignal program ampIndex =
  if ampIndex < length phases
  then
    let result = runPhaseInputProgramUntilHalt (phases !! ampIndex) inputSignal program 0
    in
      amplify phases (getDiagnosticCode result) program (succ ampIndex)
  else
    let result = runPhaseInputProgramUntilHalt (phases !! pred ampIndex) inputSignal program 0 in
      result

feedback :: [Phase] -> Int -> Program -> Int -> [Int] -> Bool -> ([Int],Int,[Int])
feedback phases input program ampIndex indexes looping =
  let
    currentAmpIndex = indexes !! ampIndex
    ampIndex' = if succ ampIndex < length phases then succ ampIndex else 0
    looping' = looping || (succ ampIndex == length phases)
    result = if looping
             then runInputProgramUntilResult input program currentAmpIndex
             else runPhaseInputProgramUntilResult (phases !! ampIndex) input program currentAmpIndex
    indexes' = setAt ampIndex (getInstructionPointer result) indexes
    in
    if ampIndex == 4 && getInstructionPointer result == 99999
    then result
    else feedback phases (getDiagnosticCode result) (getProgram result) ampIndex' indexes' looping'


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
    firstArgImmediate = xs !! (index + 1)
    secondArgPos = xs !! (xs !! (index + 2))
    secondArgImmediate = xs !! (index + 2)
    thirdArg = xs !! (index + 3)
    args = case digits opCode of
      [1] -> firstArgPos + secondArgPos
      [2] -> firstArgPos * secondArgPos
      [3] -> firstArgPos
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
      | otherwise = length xs
    xs' = case digits opCode of
      [1] -> setAt thirdArg args xs
      [1, 0, 1] -> setAt thirdArg args xs
      [1, 1, 0, 1] -> setAt thirdArg args xs
      [1, 0, 0, 1] -> setAt thirdArg args xs
      [2] -> setAt thirdArg args xs
      [1, 0, 2] -> setAt thirdArg args xs
      [1, 0, 0, 2] -> setAt thirdArg args xs
      [1, 1, 0, 2] -> setAt thirdArg args xs
      [3] -> setAt secondArgImmediate args xs
      [4] -> xs
      [1, 0, 4] -> xs
      [5] -> xs
      [1, 0, 5] -> xs
      [1, 0, 0, 5] -> xs
      [1, 1, 0, 5] -> xs
      [6] -> xs
      [1, 0, 6] -> xs
      [1, 0, 0, 6] -> xs
      [1, 1, 0, 6] -> xs
      [7] -> setAt thirdArg args xs
      [1, 0, 7] -> setAt thirdArg args xs
      [1, 0, 0, 7] -> setAt thirdArg args xs
      [1, 1, 0, 7] -> setAt thirdArg args xs
      [8] -> setAt thirdArg args xs
      [1, 0, 8] -> setAt thirdArg args xs
      [1, 0, 0, 8] -> setAt thirdArg args xs
      [1, 1, 0, 8] -> setAt thirdArg args xs
      [9, 9] -> xs
--      _ -> digits opCode ++ [999]
    acc'
      | opCode == 4 =
        acc ++ [xs !! (xs !! succ index)] -- position mode first arg
      | opCode == 104 =
        acc ++ [xs !! (xs !! (xs !! succ index))] -- immediate mode first arg
      | otherwise = opCode : acc

--bunch of types to make the intcode machine nicer to work with------------------

--setPhaseAndInput :: Phase -> InputSignal -> Program -> Program
--setPhaseAndInput phase inputSignal program =
--  let
--    inputIndices = take 2 $ elemIndices 3 program
--    in
--  setAt (program !! succ (head inputIndices)) phase program &
--  setAt (program !! succ (last inputIndices)) inputSignal
--
--setInput :: InputSignal -> Program -> Program
--setInput inputSignal program =  let
--    inputIndex = head $ elemIndices 3 program
--    in
--      setAt (program !! succ inputIndex) inputSignal program
--
runPhaseInputProgramUntilHalt :: Phase -> InputSignal -> Program -> Int -> ([Int],Int, [Int])
runPhaseInputProgramUntilHalt phase inputSignal program index =
  IC.intcodeUntilHalt program index [phase, inputSignal] []

runInputProgramUntilHalt :: InputSignal -> Program -> Int -> ([Int], Int,[Int])
runInputProgramUntilHalt inputSignal program index =
  IC.intcodeUntilHalt program index [inputSignal] []  

runPhaseInputProgramUntilResult :: Phase -> InputSignal -> Program -> Int -> ([Int],Int, [Int])
runPhaseInputProgramUntilResult phase inputSignal program index =
  IC.intcode program index [phase, inputSignal] []

runInputProgramUntilResult :: InputSignal -> Program -> Int -> ([Int], Int,[Int])
runInputProgramUntilResult inputSignal program index =
  IC.intcode program index [inputSignal] []  


--runProgramID id =
--  slurp (provideInput id input) 2 []
--
--provideInput programID xs = setAt (xs !! 1) programID xs

type Program = [Int]

type Output = [Int]

type Index = Int

type Phase = Int

type InputSignal = Int

getFinalProgram :: Result -> Output
getFinalProgram (Result (program, _)) = program

getProgram :: ([Int],Int,[Int]) -> [Int]
getProgram (program,_,_)= program

-- 0 means the test was successful. Non-zero outputs mean that a function is not working correctly;
-- If all outputs were zero except the diagnostic code, the diagnostic program ran successfully.
getOutput :: ([Int],Int,[Int]) -> [Int]
getOutput (_,_,outputs)= outputs

getInstructionPointer :: ([Int],Int,[Int]) -> Int
getInstructionPointer (_,instructionPointer,_) = instructionPointer


-- FINALLY it will output a diagnostic code and immediately halt. An output followed immediately by a halt means the program finished.
getDiagnosticCode :: ([Int],Int,[Int]) -> Int
getDiagnosticCode (_,_,outputs) = last outputs

--(example)input below----------------------------------------------------------

-- Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0)
exampleInput1 :: [Int]
exampleInput1 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]

--Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
exampleInput2 :: [Int]
exampleInput2 = [3,23,3,24,1002,24,10,24,1002,23,-1,23, 101,5,23,23,1,24,23,23,4,23,99,0,0]

--Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):

exampleInput3 :: [Int]
exampleInput3 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

--Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):
exampleInput4 :: [Int]
exampleInput4 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

--Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):
exampleInput5 :: [Int]
exampleInput5 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

input :: [Int]
input = [3,8,1001,8,10,8,105,1,0,0,21,46,59,80,105,122,203,284,365,446,99999,3,9,102,3,9,9,1001,9,5,9,102,2,9,9,1001,9,3,9,102,4,9,9,4,9,99,3,9,1002,9,2,9,101,2,9,9,4,9,99,3,9,101,5,9,9,1002,9,3,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,4,9,1001,9,2,9,102,4,9,9,101,3,9,9,102,2,9,9,4,9,99,3,9,102,5,9,9,101,4,9,9,102,3,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99]
