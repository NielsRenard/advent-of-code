{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day13
  (
  )
where

import Data.Char
import Data.Tuple
import Data.Bifunctor
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
      sortedSchedule           = sortBy (comparing fst) $ generateScheduleUpTo busIds (earliestTime + maximum busIds)
      (nextBusTime, nextBusId) = head $ dropWhile (\(time, busId) -> (earliestTime > time)) $ sortedSchedule
  in
    -- ID of the earliest bus multiplied by number of minutes you'll need to wait for that bus
    (nextBusId) * (nextBusTime - earliestTime)
  where
    splitByCommaDropXsParseToInt input =
      map (read :: String -> Int) $ filter (/= "x") $ Split.splitOn "," $ last input

-- Generates the bus schedule up to a certain time
-- inclusive
generateScheduleUpTo :: [Int] -> Int -> [(Int, Int)]
generateScheduleUpTo busIds upTo =
  concatMap (\interval -> zip [0,interval..upTo] (repeat interval)) busIds


-- Generates the bus schedule up to a certain time
generateScheduleFromTo :: [Int] -> Int -> Int -> [(Int, Int)]
generateScheduleFromTo busIds from to =
  concatMap (\interval -> zip [from,interval..to] (repeat interval)) busIds
{- "However, with so many bus IDs in your list, surely the actual earliest timestamp will be larger than 100000000000000!" -}


--solvePart2 :: [String] -> Int
solvePart2 input =
  let earliestTime      = read (head input) :: Int
      bussesWithOffsets :: [(Int, Int)] =
        map (first (read :: String -> Int)) $ map swap $ filter (\(a,b)-> b /= "x") $ indexed $ Split.splitOn "," $ last input
      bussIds = map fst bussesWithOffsets
      schedule = generateScheduleFromTo bussIds 0 1000000000000000
      sortedSchedule = sortBy (comparing fst) schedule
  in
    sortedSchedule

-- [7, 5]
-- A = 7
-- B = 5
--          10        20        30        40         50
-- 012345678901234567890123456789012345678901234567890
-- A------A------A------A------A------A------A------A-
-- B----B----B----B----B----B----B----B----B----B----B
-- ↑         ↑                        ↑              ↑
-- t0        ta                       tb             tb
--
-- ta: A = 7, B = 15
-- tb: A = 35, B = 35
-- tb: A = 49, B = 50




{- The earliest timestamp that matches the list 17,x,13,19 is 3417.
   ((/= 3417). fst) $ sortBy (comparing fst) $ generateSchedule [17,13,19] 3425
-}

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/13.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp
  let answer2 = solvePart2 input
--  putStrLn $ "Example 1: " <> show ex1     
--  putStrLn $ "   Part 1: " <> show answer1 
  putStrLn $ "Example 2: " <> show ex2    
--  putStrLn $ "   Part 2: " <> show answer2
  
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

-- takeWhileInclusive (\it-> it `mod` 13 /= 0 ) $ drop 1 $ [0,37..]
-- [37,74,111,148,185,222,259,296,333,370,407,444]
