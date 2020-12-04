{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day01 where

import Data.List
import Utils
import Debug.Trace (trace)
import Data.Maybe

part1 :: [Int] -> Maybe Int
part1 input =
  find (/= 0) [if x + y == 2020 then x * y else 0 | x <- input, y <- input]

part2 :: [Int] -> Maybe Int
part2 input =
  find (/= 0) [if x + y + z == 2020 then x * y * z else 0 | x <- input, y <- input, z <- input]

exinp :: [Int]
exinp = [1721, 979, 366, 299, 675, 1456]

input :: IO [Int]
input = map read . lines <$> readFile "data/2020/1.input"

main = do
  input <- map read . lines <$> readFile "data/2020/1.input"
  let ex1 = part1 exinp
  let answer1 = part1 input
  let ex2 = part2 exinp  
  let answer2 = part2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "Example 2: " <> show ex2  
  putStrLn $ "   Part 1: " <> show (fromJust answer1)
  putStrLn $ "   Part 2: " <> show (fromJust answer2)
