{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day20
  (
  )
where

import Data.List as L
import Data.List.Split as Split
import Data.List.Index
import Data.Maybe
import Debug.Trace (trace)
import System.Directory
import Data.Void
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar, alphaNumChar, char, separatorChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String


{- part 1
-}
 
solvePart1 input =input

main :: IO ()
main = do
  input <- readFile "data/2020/20.input"
  let ex1 = solvePart1 exinp
  putStrLn $ show ex1
--  let answer1 = solvePart1 input
--  let ex2 = solvePart2 exinp2
--  let answer2 = solvePart2 input
--  putStrLn $ "Example 1: " <> show ex1
--  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp = ""
