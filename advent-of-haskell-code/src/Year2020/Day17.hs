{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day17
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
  Simulate six cycles of three dimensional conway game.
  Then check how many active.

  Active   with 2    or  3 neighbors active  -> remain Active
  Active   with 1    or >3 neighbors active -> become Inactive

  Inactive wih 3 neighbors active           -> become Active
  Inactive with 1,2  or >3 neighbors active -> remain Inactive
-}

solvePart1 input =
  lines input

main :: IO ()
main = do
  input <- readFile "data/2020/17.input"
  let ex1 = solvePart1 exinp
--  let answer1 = solvePart1 input
--  let ex2 = solvePart2 exinp2
--  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
--  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: String
exinp = ".#.\n\
        \..#\n\
        \###"

input :: String
input = "#.#..#.#\n\
        \#.......\n\
        \####..#.\n\
        \.#.#.##.\n\
        \..#..#..\n\
        \###..##.\n\
        \.#..##.#\n\
        \.....#.."

{- printing functions -}

slicePrint :: String -> IO ()
slicePrint = putStrLn
