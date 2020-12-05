{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day05 where

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

type Row = Int

type Column = Int

type Seat = (Row, Column)

-- Alternative solution, interpreting the boarding pass as binary
solvePart1Binary :: [Text] -> Int
solvePart1Binary input =
  let binaryLists :: [[Int]] = L.map passToBinary input
      seatIds :: [Int] = L.map Utils.binaryToDecimal binaryLists
   in maximum seatIds

passToBinary :: Text -> [Int]
passToBinary inp =
  Utils.stringToInts $
    T.replace "R" "1" $
      T.replace "L" "0" $
        T.replace "B" "1" $
          T.replace "F" "0" inp

-- Every seat also has a unique seat ID: multiply the row by 8, then
-- add the column. In this example, the seat has ID 44 * 8 + 5 = 357.

-- | findHighestSeatId
solvePart1 :: [Text] -> Int
solvePart1 input =
  let allBookedSeats :: [Seat] = map (\pass -> parseBoardingPass pass) input
   in last $ sort $ map seatId allBookedSeats
  where
    seatId (row, col) = (row * 8) + col

-- | findMissingseat
solvePart2 :: [Text] -> Int
solvePart2 input =
  let allBookedSeats :: [Seat] = map (\pass -> parseBoardingPass pass) input
      allBookedSeatIds = sort $ map seatId allBookedSeats
      allSeats = [head allBookedSeatIds .. last allBookedSeatIds]
   in head $ allSeats \\ allBookedSeatIds
  where
    seatId (row, col) = (row * 8) + col

parseBoardingPass :: Text -> Seat
parseBoardingPass pass =
  let (rowInstructions, columnInstructions) = (T.take 7 pass, T.drop 7 pass)
   in (binarySplit rowInstructions [0 .. 127], binarySplit columnInstructions [0 .. 7])

binarySplit :: Text -> [Int] -> Int
binarySplit "" rows = head rows
binarySplit instructions rows =
  let (lowerHalf, upperHalf) = splitInHalf rows
   in if (T.head instructions `elem` ['F', 'L'])
        then binarySplit (T.tail instructions) $ lowerHalf
        else binarySplit (T.tail instructions) $ upperHalf

splitInHalf :: [Int] -> ([Int], [Int])
splitInHalf = splitAt =<< ((`div` 2) . L.length)

-- <2020-12-05 za 12:12> 937 too low, didn't read puzzle properly
-- <2020-12-05 za 12:18> 938 correct, funny that it was off by exactly 1
main :: IO ()
main = do
  input <- map T.pack <$> lines <$> readFile "data/2020/5.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let answer1Alt = solvePart1Binary input
  let answer2 = solvePart2 input
  putStrLn $ " Example 1: " <> show ex1
  putStrLn $ "    Part 1: " <> show answer1
  putStrLn $ "Alt Part 1: " <> show answer1Alt
  putStrLn $ "    Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [Text]
exinp =
  [ "FBFBBFFRLR", -- row  44, column 5, seat ID 357.
    "BFFFBBFRRR", -- row  70, column 7, seat ID 567.
    "FFFBBBFRRR", -- row  14, column 7, seat ID 119.
    "BBFFBBFRLL" -- row 102, column 4, seat ID 820.
  ]
