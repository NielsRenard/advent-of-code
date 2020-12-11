{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Year2020.Day11 where

import Data.Function ((&))
import Data.Maybe
import Matrix as M

data Tile = EmptySeat | OccupiedSeat | Floor deriving (Show, Eq)
type Matrix = [[Tile]]

-- parsing functions

parseTile :: Char -> Tile
parseTile =
  \case
    'L' -> EmptySeat
    '#' -> OccupiedSeat
    '.' -> Floor

parseInputToMatrix :: [String] -> Matrix
parseInputToMatrix input =
  map (map parseTile) input

-- part 1 solution

solvePart1 :: [String] -> Int
solvePart1 input =
  let matrix = parseInputToMatrix input
  in
    length $ filter (== OccupiedSeat) $ concat $ loopUntilStable matrix []
    
loopUntilStable :: Matrix -> [Matrix] -> Matrix
loopUntilStable matrix accum =
  let stable = matrix `elem` accum
  in  
    if stable
    then matrix
    else loopUntilStable (advanceGrid matrix) (matrix : accum)

advanceGrid :: Matrix -> Matrix
advanceGrid matrix =
  let
    height = length matrix
    width  = length $ head matrix
  in
    map (\y -> 
           map (\x ->
                  advanceTile (fromJust $ M.lookup x y matrix) (M.adjacent8 x y matrix)
               )
           [0..pred width])
    [0..pred height]
  

advanceTile :: Tile -> [Tile] -> Tile
advanceTile tile adjacents =
  let adjacentsEmpty    = filter (== EmptySeat) adjacents
      adjacentsOccupied = filter (== OccupiedSeat) adjacents
  in 
  case tile of
    OccupiedSeat -> if (length adjacentsOccupied) >= 4 then EmptySeat else OccupiedSeat
    EmptySeat    -> if (null adjacentsOccupied)        then OccupiedSeat else EmptySeat
    Floor        -> Floor

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/11.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
--  let ex2 = solvePart2 exinp                         
--  let answer2 = solvePart2 input                     -- *Year2020.Day11> main            
  putStrLn $ "Example 1: " <> show ex1                 -- Example 1: 37                    
  putStrLn $ "   Part 1: " <> show answer1             --    Part 1: 2126                  
--  putStrLn $ "Example 2: " <> show ex2               -- (14.61 secs, 5,112,938,080 bytes) <- sloooow
--  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [String]
exinp =
  [
    "L.LL.LL.LL",
    "LLLLLLL.LL",
    "L.L.L..L..",
    "LLLL.LL.LL",
    "L.LL.LL.LL",
    "L.LLLLL.LL",
    "..L.L.....",
    "LLLLLLLLLL",
    "L.LLLLLL.L",
    "L.LLLLL.LL"
  ]

-- printing functions

showTile :: Tile -> Char
showTile =
  \case
    EmptySeat    -> 'L' 
    OccupiedSeat -> '#' 
    Floor        -> '.' 

showGrid :: Matrix -> IO ()
showGrid matrix = do
  mapM_ putStrLn $ map (map showTile) matrix

    
