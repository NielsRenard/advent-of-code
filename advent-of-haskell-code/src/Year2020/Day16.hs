{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day16
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

data TicketField = TicketField { fieldName :: String
                               , minimumA  :: Int
                               , maximumA  :: Int
                               , minimumB  :: Int
                               , maximumB  :: Int
                               } deriving (Show)

-- parseMaybe ticketFieldParser "departure station: 29-874 or 891-961"
-- => Just (TicketField {fieldName = "departure station", minimumA = 29, maximumA = 874, minimumB = 891, maximumB = 961})
ticketFieldParser :: Parser TicketField
ticketFieldParser = do
  fieldName <- some (alphaNumChar <|> char ' ')
  colon <- char ':'
  space
  firstRangeMinimum <- decimal
  dash <- char '-'
  firstRangeMaximum <- decimal  
  space
  orString <- string "or"
  space
  secondRangeMinimum <- decimal
  dash <- char '-'
  secondRangeMaximum <- decimal    
  return $ TicketField { fieldName = fieldName
                       , minimumA = firstRangeMinimum
                       , maximumA = firstRangeMaximum
                       , minimumB = secondRangeMinimum
                       , maximumB = secondRangeMaximum}

{- part 1
   Check if each of the numbers on the ticket, fits in any of the fields.
   Sum up all the values that don't.
-}
 
solvePart1 input =
  let splitInput                    = Split.splitWhen (== "") $ Split.splitOn "\n" input
      myTicket     :: [Int]         = map read $ Split.splitOn "," (splitInput !! 1 !! 1)
      otherTickets :: [[Int]]       = map (map read) $ map (Split.splitOn ",") $ tail (splitInput !! 2)
      ticketFields :: [TicketField] = mapMaybe (parseMaybe ticketFieldParser) (splitInput !! 0)
  in
    sum $ concat $ map (\ticket -> validTicket ticket ticketFields) otherTickets

-- something strange is going on with this type signature
-- it clearly returns [[Int]] but the compiler accepts [Int]
validTicket :: [Int] -> [TicketField] -> [Int]
validTicket ticket fields =
  map (\i ->
          if (or $ map (validForField i) fields) -- I think this is causing the strange behaviour
          then 0
          else i
      ) ticket
validForField :: Int -> TicketField -> Bool
validForField x ticketField =
  x `elem` [minimumA ticketField..maximumA ticketField]
  || x `elem` [minimumB ticketField..maximumB ticketField]
  

{- part 2
  Take all the numbers from a single column, see in which fields they would fit.
  At least one of them should have only one option, from there you keep narrowing it down.
-}
solvePart2 input =
  let splitInput                    = Split.splitWhen (== "") $ Split.splitOn "\n" input
      myTicket     :: [Int]         = map read $ Split.splitOn "," (splitInput !! 1 !! 1)
      otherTickets :: [[Int]]       = map (map read) $ map (Split.splitOn ",") $ tail (splitInput !! 2)
      ticketFields :: [TicketField] = mapMaybe (parseMaybe ticketFieldParser) (splitInput !! 0)
      validTickets =  filter (\ticket -> validTicket2 ticket ticketFields) otherTickets
      column1 = map (!! 0) validTickets
      column2 = map (!! 1) validTickets
      column3 = map (!! 2) validTickets
      column4 = map (!! 3) validTickets
      column5 = map (!! 4) validTickets
      column6 = map (!! 5) validTickets
      column7 = map (!! 6) validTickets
      column8 = map (!! 7) validTickets
      column9 = map (!! 8) validTickets
      column10 = map (!! 9) validTickets
      column11 = map (!! 10) validTickets
      column12 = map (!! 11) validTickets
      column13 = map (!! 12) validTickets
      column14 = map (!! 13) validTickets
      column15 = map (!! 14) validTickets
      column16 = map (!! 15) validTickets
      column17 = map (!! 16) validTickets
      column18 = map (!! 17) validTickets
      column19 = map (!! 18) validTickets
      column20 = map (!! 19) validTickets
  in
     [findPossibleFields column1 ticketFields
     , findPossibleFields column2 ticketFields
     , findPossibleFields column3 ticketFields
     , findPossibleFields column4 ticketFields     
     , findPossibleFields column5 ticketFields     
     , findPossibleFields column6 ticketFields     
     , findPossibleFields column7 ticketFields     
     , findPossibleFields column8 ticketFields     
     , findPossibleFields column9 ticketFields     
     , findPossibleFields column10 ticketFields     
     , findPossibleFields column11 ticketFields     
     , findPossibleFields column12 ticketFields     
     , findPossibleFields column13 ticketFields     
     , findPossibleFields column14 ticketFields     
     , findPossibleFields column15 ticketFields     
     , findPossibleFields column16 ticketFields     
     , findPossibleFields column17 ticketFields     
     , findPossibleFields column18 ticketFields     
     , findPossibleFields column19 ticketFields     
     , findPossibleFields column20 ticketFields     
     ]

{- Manual solution using this output
   column 1  = wagon
   column 2  = route
   column 3  = arrival platform
 * column 4  = departure track
 * column 5  = departure time
   column 6  = row
   column 7  = arrival location
   column 8  = seat
   column 9  = train
   column 10 = arrival track
   column 11 = seattype
   column 12 = zone
 * column 13 = departure station
   column 14 = class
 * column 15 = departure location
 * column 16 = departure platform
   column 17 = arrival station
 * column 18 = departure date
   column 19 = duration
   column 20 = price
   your ticket:
   197,173,229,179,157,83,89,79,193,53,163,59,227,131,199,223,61,181,167,191
   (*          179,157,                       227,    199,223,   181) == 51240700105297
  18151240700105297 too high <-- lmao typo'd
  51240700105297 ⋆⋆
-}

findPossibleFields :: [Int] -> [TicketField] -> [String]
findPossibleFields column fields =
  mapMaybe (\field -> if (and $ map (\i -> validForField i field ) column)
                 then (Just (fieldName field))
                 else Nothing)
  fields

validTicket2 :: [Int] -> [TicketField] -> Bool
validTicket2 ticket fields =
  and $ map or $ map (\i -> map (\field -> validForField i field) fields) ticket

main :: IO ()
main = do
  input <- readFile "data/2020/16.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp2
  let answer2 = solvePart2 input
--  putStrLn $ "Example 1: " <> show ex1
--  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: String
exinp = "class: 1-3 or 5-7\n\
          \row: 6-11 or 33-44\n\
          \seat: 13-40 or 45-50\n\
          \\n\
          \your ticket:\n\
          \7,1,14\n\
          \\n\
          \nearby tickets:\n\
          \7,3,47\n\
          \40,4,50\n\
          \55,2,2\n\
          \38,6,12\n"
        
exinp2 :: String
exinp2 = "class: 0-1 or 4-19\n\
           \row: 0-5 or 8-19\n\
           \seat: 0-13 or 16-19\n\
           \\n\
           \your ticket:\n\
           \11,12,13\n\
           \\n\
           \nearby tickets:\n\
           \3,9,18\n\
           \15,1,5\n\
           \5,14,9\n"

{-
classs = fromJust $ parseMaybe ticketFieldParser "class: 1-3 or 5-7"
seat = fromJust $ parseMaybe ticketFieldParser "seat: 13-40 or 45-50"
row = fromJust $ parseMaybe ticketFieldParser "row: 6-11 or 33-44"
-}

