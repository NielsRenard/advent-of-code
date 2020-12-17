{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day15
  (
  )
where

import Data.List as L
import Data.List.Split as Split
import Data.List.Index
import Debug.Trace (trace)
import System.Directory
import qualified Data.HashMap.Strict as HM

{- part 1 -}
solvePart1 :: [Int] -> Int
solvePart1 = last . loop


loop :: [Int] -> [Int]
loop numbers =
  let
    currentTurn             = length numbers
    previouslySpokenAtTurns = map succ $ elemIndices (last numbers) (reverse . drop 1 . reverse $ numbers)
  in
    if length numbers == 2020 -- exit condition
    then numbers
    else 
      if null previouslySpokenAtTurns
      then loop (numbers ++ [0])
      else loop (numbers ++ [currentTurn - (last previouslySpokenAtTurns)])


{- part 2 -}

solvePart2 input =
--  last $ loop2 input 5 (HM.fromList [(8,0), (13,2), (1,3), (0,4), (18,5)])
--  loop2 6 3 (HM.fromList [(0,1),(3,2)])
  loop2 9 6 (HM.fromList [(8,1),(13,2),(1,3),(0,4),(18,5)])

loop2 :: Int -> Int -> (HM.HashMap Int Int) -> Int
loop2 latestNumber turn accum =
  let
    previouslySpokenAtTurn  = HM.lookup latestNumber accum
  in
--    (trace $ show numbers) $     
    if turn == 30000000 -- exit condition
    then latestNumber
    else 
      case previouslySpokenAtTurn of
      Nothing ->
--        (trace $ show "turn" <> show turn) $
--        (trace $ show "NewNum" <> show latestNum) $
--        (trace "") $
        loop2 0 (succ turn) (HM.insert latestNumber turn accum)
      (Just previousTurn) ->
--        (trace $ show "turn" <> show turn) $
--        (trace $ show "latestNum" <> show latestNum) $        
--        (trace $ show "prevCalledOn" <> show previousTurn) $
--        (trace $ "adding " <> (show [turn - previousTurn])) $
--        (trace "") $        
        --(trace $ show [turn - previousTurn])
        loop2 (turn - previousTurn) (succ turn) (HM.insert latestNumber turn accum)



main :: IO ()
main = do
  input :: [Int] <- map read <$> splitOn "," <$> readFile "data/2020/15.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp
  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [Int]
--        0 3 6 0 3 3 1 0 4 0....
exinp = [ 0,3,6 ]

inp = [ 8,13,1,0,18,9 ]
