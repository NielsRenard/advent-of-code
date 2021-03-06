{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day09 where

import Data.String
import System.Directory
import Utils

solvePart1 :: Int -> [Int] -> Int
solvePart1 preamble input =
  getFirstNonConforming preamble input

getFirstNonConforming :: Int -> [Int] -> Int
getFirstNonConforming preamble [] = 0
getFirstNonConforming preamble xs =
  let previous = take preamble xs
      number = head $ drop preamble xs
   in if isSumOfTwoPrevious previous number
        then 0 + getFirstNonConforming preamble (drop 1 xs)
        else number

isSumOfTwoPrevious :: [Int] -> Int -> Bool
isSumOfTwoPrevious previous number =
  not $ null [True | x <- previous, y <- previous, z <- [number], x + y == z, x /= y]

type Preamble = Int

type Target = Int

solvePart2 :: Int -> [Preamble] -> Int
solvePart2 preamble input =
  let
    target                   = solvePart1 preamble input
    upToTarget               = takeWhileInclusive input (/= target)
    sequenceThatSumsToTarget = tryToSum upToTarget [] target
  in
    minimum sequenceThatSumsToTarget + maximum sequenceThatSumsToTarget

tryToSum :: [Int] -> [Int] -> Target -> [Int]
tryToSum [] bag target = []
tryToSum xs bag target =
  let
    bagPlusNext = (bag ++ [head . (drop $ length bag) $ xs])
  in
    if (sum bag) == target                   -- finished
    then bag                                 -- return bag
    else if sum (bagPlusNext) > target       -- is there still room in the bag?
         then tryToSum (drop 1 xs) [] target -- no : start counting fresh again
         else tryToSum xs bagPlusNext target -- yes: add another number to the bag

main :: IO ()
main = do
  input <- map (\s -> read s :: Int) <$> lines <$> readFile "data/2020/9.input"
  let preambleEx = 5
  let answerEx1 = solvePart1 preambleEx exinp

  let preamble = 25
  let answer1 = solvePart1 preamble input

  let answerEx2 = solvePart2 preambleEx exinp
  let answer2 = solvePart2 preamble input

  putStrLn $ "Example 1: " <> show answerEx1  -- Example 1: 127
  putStrLn $ "   Part 1: " <> show answer1    --    Part 1: 18272118
  putStrLn $ "Example 2: " <> show answerEx2  -- Example 2: 62
  putStrLn $ "   Part 2: " <> show answer2    --    Part 2: 2186361
                                              -- (0.70 secs, 1,665,136,952 bytes)


{-- Test and example input --}

exinp :: [Int]
exinp = [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
