{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Year2020.Day03 where

import Data.Function ((&))
import Data.List as L
import Data.List.Index
import qualified Data.Text as T
import Data.String
import Data.List.Split (chunksOf)
import Data.Text as T (Text, pack, unpack)
import Utils
import Debug.Trace (trace)
import System.Directory
import Data.Maybe

startPart1 input =
  solve (3, 1) 0 0 input 0

startPart2 slope input =
  solve slope 0 0 input 0


solve :: (Int, Int) -> Int -> Int -> [String] -> Int -> Int
solve slope x y input numTrees =
  let
    height       = L.length input
    width        = L.length . head $ input
    slopeX       = fst slope
    slopeY       = snd slope
    curHasTree   = hasTree x y input
    edgeDistance = width - x
    newX         = if edgeDistance <= slopeX then (slopeX - edgeDistance) else x + slopeX
    newY         = y + slopeY
    newNumTrees  =  if curHasTree then succ numTrees else numTrees
  in
    -- trace ("height:" <> show height <>
    --        ",width:" <> show width <>
    --        ",x" <> show x <>
    --        ",y" <> show y <>            
    --        ",edgeDistance" <> show edgeDistance <>
    --        ",newX:" <> show newX <>
    --        ",newY:" <> show newY <>           
    --        ",curHasTree:" <> show curHasTree <>
    --        ",numTrees:" <> show numTrees <>
    --        ",newNumTrees:" <> show newNumTrees) $
    if newY < height
    then solve slope newX newY input newNumTrees
    else newNumTrees
    where
      hasTree :: Int -> Int -> [String] -> Bool
      hasTree x y input = (input !! y) !! x == '#'



exinp =
  [
    "..##.......",
    "#...#...#..",
    ".#....#..#.",
    "..#.#...#.#",
    ".#...##..#.",
    "..#.##.....",
    ".#.#.#....#",
    ".#........#",
    "#.##...#...",
    "#...##....#",
    ".#..#...#.#"
  ]

listOfSlopes = [(1,1), (3,1), (5,1), (7,1), (1, 2)]

main :: IO ()
main = do
  listOfStrings <- lines <$> readFile "data/2020/3.input"
  let ex1 = startPart1 exinp
  let answer1 = startPart1 listOfStrings
  let ex2 = foldr (*) 1 $ L.filter (/= 0) $ L.map (\it -> startPart2 it exinp) listOfSlopes  
  let answer2 = foldr (*) 1 $ L.filter (/= 0) $ L.map (\it -> startPart2 it listOfStrings) listOfSlopes
  putStrLn $ "Example part 1: " <> show ex1
  putStrLn $ "Example part 2: " <> show ex2
  putStrLn $ "Part 1: " <> show answer1
  putStrLn $ "Part 2: " <> show answer2  
