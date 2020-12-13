{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day13
  (
  )
where

import Data.Char
import Data.Function ((&))
import Data.List as L
import Data.List.Split as Split
import Data.List.Index
import Data.Maybe
import Data.String
import Debug.Trace (trace)
import RIO hiding (many, trace, (.~), try)
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar, alphaNumChar, char, separatorChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Utils


-- data Direction = N | W |  E | S deriving (Show)

-- type Coordinate = (Int, Int)
-- type Ferry = (Coordinate, Direction)
-- type Degrees = Int

-- data Instruction = Instruction
--   deriving (Show)


-- type Parser = Parsec Void String

-- instructionParser :: Parser Instruction
-- instructionParser = do
--   instruction <- upperChar
--   value <- Lex.decimal
--   case (instruction) of
--     return Inst
  
--parseInstructions :: [String] -> [Instruction]
--parseInstructions input =
--  mapMaybe (parseMaybe instructionParser) input


solvePart1 :: [String] -> Int
solvePart1 input =
  let earliestTime             = read (head input) :: Int
      busIds                   = splitByCommaDropXsParseToInt input
      sortedSchedule           = sortBy (comparing fst) $
                                 concatMap (\interval -> zip [0,interval..(earliestTime + minimum busIds)] (repeat interval)) busIds
      (nextBusTime, nextBusId) = head $ dropWhile (\(time, busId) -> (earliestTime > time)) $ sortedSchedule
  in
    -- ID of the earliest bus multiplied by number of minutes you'll need to wait for that bus
    (nextBusId) * (nextBusTime - earliestTime)
  where
    splitByCommaDropXsParseToInt input =
      map (read :: String -> Int) $ filter (/= "x") $ Split.splitOn "," $ last input

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/13.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  putStrLn $ "Example 1: " <> show ex1     
  putStrLn $ "   Part 1: " <> show answer1 

  
{-- Test and example input --}

exinp :: [String]
exinp =
    [
      "939",
      "7,13,x,x,59,x,31,19"
    ]


inp =
  [
  "1002576",
  "13,x,x,x,x,x,x,37,x,x,x,x,x,449,x,29,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,19,x,x,x,23,x,x,x,x,x,x,x,773,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17"
  ]
