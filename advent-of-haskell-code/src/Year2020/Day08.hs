{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day08 where

import Data.Char
import Data.Function ((&))
import Data.List as L
import Data.List.Index
import qualified Data.List.Split as Split
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import RIO hiding (many, trace)
import System.Directory
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, string, space, separatorChar)
import qualified Text.Megaparsec.Char.Lexer as Lex
type Parser = Parsec Void String


solvePart1 :: [String] -> Int
solvePart1 input =
  computerPart1 (parseInstructions input) 0 (0, [])

solvePart2 :: [String] -> Accumulator
solvePart2 input =
  computer (parseInstructions input) 0 (0, [])


parseInstructions :: [String] -> [Instruction]
parseInstructions input =
  L.map (toInstruction . words) input
  where
    toInstruction line  =
      (parseOp (head line), parseArg (last line))
    parseOp s = case s of
      "acc" -> Acc
      "jmp" -> Jmp
      "nop" -> Nop
      _     -> Err
    parseArg s =
      if head s == '+'
      then read (drop 1 s) :: Int
      else read s :: Int
      

type Instruction = (Operation, Argument)
data Operation = Acc | Jmp | Nop | Err deriving (Show)
type Argument  = Int
type Accumulator = (Int, Visited)
type Visited = [Int]
type Cursor = Int

-- Part 1
-- This computer stops when it encounters an instruction it's visited before.
computerPart1 :: [Instruction] -> Cursor -> Accumulator -> Int
computerPart1 instructions cursor accumulator =
  if  cursor `elem` (snd accumulator)
  then (trace $ show accumulator) $ fst accumulator
  else go $ (instructions !! cursor)
  where
    go instruction =
      case (fst instruction) of
        Acc -> computerPart1 instructions (succ cursor) ((acc instruction), visited)
        Nop -> computerPart1 instructions (succ cursor) accumulator
        Jmp -> computerPart1 instructions (jump cursor instruction) accumulator                

    acc :: Instruction -> Int
    acc i =
      (fst accumulator) + snd i

    jump :: Cursor -> Instruction -> Cursor
    jump cursor' i =
      cursor' + (snd i)

    visited =
      (snd accumulator) ++ [cursor]

-- This computer stops when it finishes all instructions
computer :: [Instruction] -> Cursor -> Accumulator -> Accumulator
computer instructions cursor accumulator =
  if cursor >= length instructions
  then (trace $ show accumulator) $ accumulator
  else go $ (instructions !! cursor)
  where
    go instruction =
      case (fst instruction) of
        Acc -> computer instructions (succ cursor) ((acc instruction), visited)
        Nop -> computer instructions (succ cursor) accumulator
        Jmp -> computer instructions (jump cursor instruction) accumulator                

    acc :: Instruction -> Int
    acc i =
      (fst accumulator) + snd i

    jump :: Cursor -> Instruction -> Cursor
    jump cursor' i =
      cursor' + (snd i)

    visited =
      (snd accumulator) ++ [cursor]

        
      
   

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/8.input"
  let ex1 = solvePart1 exinp1
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp2
  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1

  putStrLn $ "Example 2: " <> show ex2
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp1 =
  [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -9",
    "acc +1",
    "jmp -4",
    "acc +6"
  ]

exinp2 =
  [
    "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -9",
    "acc +1",
    "nop -4",
    "acc +6"
  ]  

