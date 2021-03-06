module Year2019.Day02 where

import Data.List
import Data.List.Index

-- "replace position 1 with the value 12 and replace position 2 with the value 2."
twelveOTwo = setAt 1 12 . setAt 2 2

solvePartOne = head $ slurp (twelveOTwo input) 0

solvePartTwo = head $ bruteForcePermutations input 19690720

newtype Instruction = Instruction (Int, Int, Int, Int)

-- this function does all the work
slurp :: [Int] -> Int -> [Int]
slurp [] index = []
slurp xs index =
  if index > length xs
    then xs
    else slurp xs' index'
  where
    index' = (index + 4)
    -- clean this up, instructionFn is a bit weird
    instructionInput = getInstruction $ drop index xs
    getInstruction xs = (head xs, xs !! 1, xs !! 2, xs !! 3)
    xs' = instructionFn instructionInput xs
    instructionFn :: (Int, Int, Int, Int) -> ([Int] -> [Int])
    instructionFn (1, b, c, d) = plusCode (b, c, d)
    instructionFn (2, b, c, d) = multCode (b, c, d)
    instructionFn (99, b, c, d) = id
    instructionFn _ = id

plusCode (n1, n2, index) xs = setAt index (xs !! n1 + xs !! n2) xs

multCode (n1, n2, index) xs = setAt index (xs !! n1 * xs !! n2) xs

insertNounAndVerb n v = setAt 1 n . setAt 2 v

-- thought this would run slow, but runs in 0.66 seconds

bruteForcePermutations input desiredOutput =
  let index = 0
   in filter
        (/= 0)
        [ if desiredOutput == head (slurp (insertNounAndVerb n v input) index)
            then 100 * n + v
            else 0
          | n <- [0 .. 99],
            v <- [0 .. 99]
        ]

--(example) input below---------------------------------------------------------

exInput :: [Int]
exInput = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

exInput2 :: [Int]
exInput2 = [1, 0, 0, 0, 99]

exResult2 :: [Int]
exResult2 = [2, 0, 0, 0, 99]

exInput3 :: [Int]
exInput3 = [2, 3, 0, 3, 99]

exResult3 :: [Int]
exResult3 = [2, 3, 0, 6, 99]

input :: [Int]
input = [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 10, 19, 1, 6, 19, 23, 1, 10, 23, 27, 2, 27, 13, 31, 1, 31, 6, 35, 2, 6, 35, 39, 1, 39, 5, 43, 1, 6, 43, 47, 2, 6, 47, 51, 1, 51, 5, 55, 2, 55, 9, 59, 1, 6, 59, 63, 1, 9, 63, 67, 1, 67, 10, 71, 2, 9, 71, 75, 1, 6, 75, 79, 1, 5, 79, 83, 2, 83, 10, 87, 1, 87, 5, 91, 1, 91, 9, 95, 1, 6, 95, 99, 2, 99, 10, 103, 1, 103, 5, 107, 2, 107, 6, 111, 1, 111, 5, 115, 1, 9, 115, 119, 2, 119, 10, 123, 1, 6, 123, 127, 2, 13, 127, 131, 1, 131, 6, 135, 1, 135, 10, 139, 1, 13, 139, 143, 1, 143, 13, 147, 1, 5, 147, 151, 1, 151, 2, 155, 1, 155, 5, 0, 99, 2, 0, 14, 0]
