{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day14
  (
  )
where

import qualified Data.Vector as V
import Data.Char
import Data.Bifunctor
import Data.Function ((&))
import Data.List as L
import Data.List.Index
import Data.Maybe
import Data.String
import Debug.Trace (trace)
import RIO hiding (some, many, trace, (.~), try)
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar, alphaNumChar, char, separatorChar, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils
type Parser = Parsec Void String

type Address = Int
type Index = Int
type Value = Int
type Bitmask = [(Index, Value)]

data Instruction = UpdateBitmask Bitmask | WriteToMemory (Address, Value) deriving (Show)

{- parsers -}

-- parseMaybe updateBitmaskParser "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
-- => Just (UpdateBitmask [(29,1),(34,0)])
updateBitmaskParser :: Parser Instruction
updateBitmaskParser = do
  mask <- string "mask"
  space
  equalsSign <- char '='
  space
  bitmaskString <- some alphaNumChar
  return $ UpdateBitmask $ toBitmask $ bitmaskString

-- parseMaybe writeToMemoryParser "mem[7] = 101"
-- => Just (WriteToMemory (7,101))
writeToMemoryParser :: Parser Instruction
writeToMemoryParser = do
  mem <- string "mem"
  openBracket <- char '['
  address <- decimal
  closeBracket <- char ']'
  space
  equalsSign <- char '='
  space
  value <- decimal
  return $ WriteToMemory (address, value)

{- conversion -}
toBitmask :: String -> [(Index, Value)]
toBitmask s =
  map (second digitToInt) $
  filter (\(a,v) -> v /= 'X') $ indexed s

toThirtysixBits :: Int -> Vector Int
toThirtysixBits = V.fromList . leftPadZeroes 36 . toBinary


solvePart1 input =
  let instructions =
        mapMaybe (parseMaybe (try updateBitmaskParser <|> writeToMemoryParser)) input
      initInstruction = (head instructions)
--      memoryInstructions = [ mem | mem@(WriteToMemory _) <- instructions]
      initMemory = V.fromList (take 100000 (repeat 0))
  in
    case initInstruction of
      UpdateBitmask bitmask -> 
        V.sum $ loop (tail instructions) bitmask initMemory
      _ ->
        0

loop :: [Instruction] -> Bitmask -> Vector Int -> Vector Int
loop [] _ memory = memory
loop instructions@(x:xs) bitmask memory =
  case x of
    UpdateBitmask bitmask' ->
      loop xs bitmask' memory
    WriteToMemory (address, value) ->
      let maskedBits = ((toThirtysixBits value) V.// bitmask)
          newValue =  fromBinary $ toList maskedBits
      in
        loop xs bitmask (V.update memory $ V.fromList [(address, newValue)])


main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/14.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1

{-- Test and example input --}

exinp :: [String]
exinp =
    [
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    ]

--too low 6453111245
