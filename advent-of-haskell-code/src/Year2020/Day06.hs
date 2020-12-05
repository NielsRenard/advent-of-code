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
  let x = 1
   in f x
  where
    f = id

solvePart2 input =
  let x = 1
   in f x
  where
    f = id

main :: IO ()
main = do
  input <- map T.pack <$> lines <$> readFile "data/2020/6.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [Text]
exinp =
  [ "STUB"
  ]
