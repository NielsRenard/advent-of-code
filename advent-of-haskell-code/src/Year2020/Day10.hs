{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day10 where

import Data.List as L
import Data.Maybe
import Data.String
import System.Directory
import qualified Data.HashMap.Strict as HM

solvePart1 :: [Int] -> Int
solvePart1 input =
  let builtInAdapter = (maximum input) + 3
      sorted         = 0 : sort input ++ [builtInAdapter]
      differences    = zipWith (-) (tail sorted) $ sorted
      ones           = filter (== 1) differences
      threes         = filter (== 3) differences
  in
    length ones * length threes

solvePart2 :: [Int] -> Maybe Int
solvePart2 input =                                     -- e.g. [4,1,3]
  let sorted         = sort input                      --      [1,3,4]
      builtInAdapter = ((maximum input) + 3)           --      7
      allAdapters    = (0 : sorted) ++ [builtInAdapter]--      [0,1,3,4,7]
      newAccumulator = HM.singleton 0 1                -- there is 1 'path' to reach 0
  in
    HM.lookup builtInAdapter $                         -- lookup the # of paths to reach device
    loop allAdapters newAccumulator


-- We loop over the adapters, checking how many others each can reach.
-- Every time we can reach something, we mark that in our accumulator.
--
-- Why this works:
-- The number of ways to reach an adapter, is the sum of the number
-- of ways to reach the adapters leading up to it.
loop :: [Int] -> (HM.HashMap Int Int) -> (HM.HashMap Int Int)
loop [] accMap = accMap
loop (x:xs) accMap =
    loop xs $
      updateIfPresent x (x+3) xs $               -- There is a nice way
        updateIfPresent x (x+2) xs $             -- to compose these three functions
          updateIfPresent x (x+1) xs accMap      -- but I am tired

updateIfPresent :: Int -> Int -> [Int] -> (HM.HashMap Int Int) -> (HM.HashMap Int Int)
updateIfPresent x y list accMap =
  let
    numberOfWaysToReachCurrent = fromJust $ HM.lookup x accMap
  in
    -- Check if element y exists in the list
    if y `elem` list
    -- If yes: add the number of ways to reach the current element, to the next one
    then (HM.insertWith (+) y numberOfWaysToReachCurrent accMap)
    -- Otherwise return the accumulator
    else accMap

main :: IO ()
main = do
  input <- map (\s -> read s :: Int) <$> lines <$> readFile "data/2020/10.input"
  let ex1 = solvePart1 exinp2
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp
  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
  putStrLn $ "Example 2: " <> show ex2
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [Int]
exinp =  [ 16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4 ]

exinp2 :: [Int]
exinp2 = [ 28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19, 38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3]

inp :: [Int]
inp = [84,60,10,23,126,2,128,63,59,69,127,73,140,55,154,133,36,139,4,70,110,97,153,105,41,106,79,145,35,134,146,148,13,77,49,107,46,138,88,152,83,120,52,114,159,158,53,76,16,28,89,25,42,66,119,3,17,67,94,99,7,56,85,122,18,20,43,160,54,113,29,130,19,135,30,80,116,91,161,115,141,102,37,157,129,34,147,142,151,68,78,24,90,121,123,33,98,1,40]

exinpAdditional4a :: [Int]
exinpAdditional4a = [10, 6, 4, 7, 1, 5]

exinpAdditional4b :: [Int]
exinpAdditional4b = [ 3, 1, 6, 2]

exinpAdditional7 :: [Int]
exinpAdditional7 = [ 4, 11, 7, 8, 1, 6, 5]

exinpAdditional28 :: [Int]
exinpAdditional28 = [ 17, 6, 10, 5, 13, 7, 1, 4, 12, 11, 14]
