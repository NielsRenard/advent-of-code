{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day06 where

import Data.Char
import Data.Function ((&))
import Data.List as L
import Data.List.Index
import Data.List.Split (splitWhen)
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import RIO (comparing)
import System.Directory
import Utils

solvePart1 input =
  let uniqueAnswersPerGroup =
        L.map (S.fromList . T.unpack)
        $ L.map (T.replace " " "" )
        $ L.map T.unwords
        $ splitWhen (== "") input
  in
    L.foldr ((+) . L.length) 0 uniqueAnswersPerGroup

solvePart2 input =
  let answersPerGroup :: [[Text]] = splitWhen (== "") input
      listOfListsOfAnswers :: [[String]] = (L.map (L.map T.unpack)) answersPerGroup
  in
    foldr (+) 0
    $ L.map L.length
    $ L.map (L.foldl1 S.intersection)
    $ L.map (L.map S.fromList) listOfListsOfAnswers

main :: IO ()
main = do
  input <- map T.pack <$> lines <$> readFile "data/2020/6.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp  
  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
  putStrLn $ "Example 2: " <> show ex2  
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [Text]
exinp =
  [
    "abc",
    "",
    "a",
    "b",
    "c",
    "",
    "ab",
    "ac",
    "",
    "a",
    "a",
    "a",
    "a",
    "",
    "b"
  ]
