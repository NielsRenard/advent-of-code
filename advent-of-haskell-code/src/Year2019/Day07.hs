module Year2019.Day07 where

-- this puzzle continues from Day05

import Data.Function ((&))
import Data.List
import Data.List.Index
import qualified Data.Set as Set hiding (take)
import Data.Tuple.Utils
import Utils
import qualified Year2019.Intcode as IC

--solvePartOne :: Program -> Output
solvePartOne input =
  maximum $
    [ getOutput $ amplify [a, b, c, d, e] 0 input 0
      | a <- [0 .. 4],
        b <- [0 .. 4],
        c <- [0 .. 4],
        d <- [0 .. 4],
        e <- [0 .. 4],
        (Set.fromList [a, b, c, d, e] & Set.size) == 5
    ]

solvePartTwo :: Program -> Int
solvePartTwo input =
  maximum $
    [ (maximum . getOutput) $ feedback [a, b, c, d, e] 0 (replicate 5 input) 0 [0, 0, 0, 0, 0] False []
      | a <- [5 .. 9],
        b <- [5 .. 9],
        c <- [5 .. 9],
        d <- [5 .. 9],
        e <- [5 .. 9],
        (Set.fromList [a, b, c, d, e] & Set.size) == 5
    ]

amplify :: [Phase] -> Int -> Program -> Int -> ([Int], Int, [Int])
amplify phases inputSignal program ampIndex =
  if ampIndex < length phases
    then
      let result = runPhaseInputProgramUntilHalt (phases !! ampIndex) inputSignal program 0
       in amplify phases (getDiagnosticCode result) program (succ ampIndex)
    else
      let result = runPhaseInputProgramUntilHalt (phases !! pred ampIndex) inputSignal program 0
       in result

feedback :: [Phase] -> Int -> [Program] -> Int -> [Int] -> Bool -> [Int] -> ([Int], Int, [Int])
feedback phases input programs ampIndex indexes looping outputs =
  let currentAmpIndex = indexes !! ampIndex
      ampIndex' = if succ ampIndex < length phases then succ ampIndex else 0
      looping' = looping || (succ ampIndex == length phases)
      result =
        if looping
          then runInputProgramUntilResult input (programs !! ampIndex) currentAmpIndex outputs
          else runPhaseInputProgramUntilResult (phases !! ampIndex) input (programs !! ampIndex) currentAmpIndex outputs
      indexes' = setAt ampIndex (getInstructionPointer result) indexes
      outputs' = outputs ++ [getDiagnosticCode result]
      programs' = setAt ampIndex (getProgram result) programs
   in if ampIndex == 4 && getInstructionPointer result == 99999
        then result
        else feedback phases (getDiagnosticCode result) programs' ampIndex' indexes' looping' outputs'

runPhaseInputProgramUntilHalt :: Phase -> InputSignal -> Program -> Int -> ([Int], Int, [Int])
runPhaseInputProgramUntilHalt phase inputSignal program index =
  IC.intcodeUntilHalt program index [phase, inputSignal] []

runInputProgramUntilHalt :: InputSignal -> Program -> Int -> ([Int], Int, [Int])
runInputProgramUntilHalt inputSignal program index =
  IC.intcodeUntilHalt program index [inputSignal] []

runPhaseInputProgramUntilResult :: Phase -> InputSignal -> Program -> Int -> [Int] -> ([Int], Int, [Int])
runPhaseInputProgramUntilResult phase inputSignal program index outputs =
  IC.intcode program index [phase, inputSignal] outputs

runInputProgramUntilResult :: InputSignal -> Program -> Int -> [Int] -> ([Int], Int, [Int])
runInputProgramUntilResult inputSignal program index outputs =
  IC.intcode program index [inputSignal] outputs

type Program = [Int]

type Output = [Int]

type Index = Int

type Phase = Int

type InputSignal = Int

getProgram :: ([Int], Int, [Int]) -> [Int]
getProgram (program, _, _) = program

-- 0 means the test was successful. Non-zero outputs mean that a function is not working correctly;
-- If all outputs were zero except the diagnostic code, the diagnostic program ran successfully.
getOutput :: ([Int], Int, [Int]) -> [Int]
getOutput (_, _, outputs) = outputs

getInstructionPointer :: ([Int], Int, [Int]) -> Int
getInstructionPointer (_, instructionPointer, _) = instructionPointer

-- FINALLY it will output a diagnostic code and immediately halt. An output followed immediately by a halt means the program finished.
getDiagnosticCode :: ([Int], Int, [Int]) -> Int
getDiagnosticCode (_, _, outputs) = last outputs

--(example)input below----------------------------------------------------------

-- Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0)
exampleInput1 :: [Int]
exampleInput1 = [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0]

--Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
exampleInput2 :: [Int]
exampleInput2 = [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0]

--Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):

exampleInput3 :: [Int]
exampleInput3 = [3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0]

--Max thruster signal 139629729 (from phase setting sequence 9,8,7,6,5):
exampleInput4 :: [Int]
exampleInput4 = [3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26, 27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5]

--Max thruster signal 18216 (from phase setting sequence 9,7,8,5,6):
exampleInput5 :: [Int]
exampleInput5 = [3, 52, 1001, 52, -5, 52, 3, 53, 1, 52, 56, 54, 1007, 54, 5, 55, 1005, 55, 26, 1001, 54, -5, 54, 1105, 1, 12, 1, 53, 54, 53, 1008, 54, 0, 55, 1001, 55, 1, 55, 2, 53, 55, 53, 4, 53, 1001, 56, -1, 56, 1005, 56, 6, 99, 0, 0, 0, 0, 10]

input :: [Int]
input = [3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 46, 59, 80, 105, 122, 203, 284, 365, 446, 99999, 3, 9, 102, 3, 9, 9, 1001, 9, 5, 9, 102, 2, 9, 9, 1001, 9, 3, 9, 102, 4, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 101, 5, 9, 9, 1002, 9, 3, 9, 1001, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 1002, 9, 4, 9, 1001, 9, 2, 9, 102, 4, 9, 9, 101, 3, 9, 9, 102, 2, 9, 9, 4, 9, 99, 3, 9, 102, 5, 9, 9, 101, 4, 9, 9, 102, 3, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 99]
