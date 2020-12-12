{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Year2020.Day11 where

import Data.Function ((&))
import Data.Maybe
import Data.List
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
    else loopUntilStable (advanceGrid matrix) [matrix]

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
    Floor        -> Floor
    OccupiedSeat -> if (length adjacentsOccupied) >= 4 then EmptySeat else OccupiedSeat
    EmptySeat    -> if (null adjacentsOccupied)        then OccupiedSeat else EmptySeat


-- part 2 solution

solvePart2 :: [String] -> Int
solvePart2 input =
  let matrix = parseInputToMatrix input
  in
    length $ filter (== OccupiedSeat) $ concat $ loopUntilStable2 matrix []


loopUntilStable2 :: Matrix -> [Matrix] -> Matrix
loopUntilStable2 matrix accum =
  let stable = matrix `elem` accum
  in
    if stable
    then matrix
    else loopUntilStable2 (advanceGrid2 matrix) [matrix]

advanceGrid2 :: Matrix -> Matrix
advanceGrid2 matrix =
  let
    height = length matrix
    width  = length $ head matrix
  in
    map (\y ->
           map (\x ->
                  advanceTile2 (fromJust $ M.lookup x y matrix) (nonFloorsInSight x y matrix)
               )
           [0..pred width])
    [0..pred height]


nonFloorsInSight :: Int -> Int -> Matrix -> [Tile]
nonFloorsInSight x y matrix =
  let firstSightNW = find (/= Floor) $ lookupTiles (M.lineOfSightNorthWest x y matrix) matrix
      firstSightN  = find (/= Floor) $ lookupTiles (M.lineOfSightNorth x y matrix)     matrix
      firstSightNE = find (/= Floor) $ lookupTiles (M.lineOfSightNorthEast x y matrix) matrix
      firstSightW  = find (/= Floor) $ lookupTiles (M.lineOfSightWest x y matrix)      matrix
      firstSightE  = find (/= Floor) $ lookupTiles (M.lineOfSightEast x y matrix)      matrix
      firstSightSW = find (/= Floor) $ lookupTiles (M.lineOfSightSouthWest x y matrix) matrix
      firstSightS  = find (/= Floor) $ lookupTiles (M.lineOfSightSouth x y matrix)     matrix
      firstSightSE = find (/= Floor) $ lookupTiles (M.lineOfSightSouthEast x y matrix) matrix
  in
    catMaybes $ [firstSightNW, firstSightN ,firstSightNE,
                 firstSightW               ,firstSightE,
                 firstSightSW, firstSightS ,firstSightSE ]
--  where
lookupTiles :: [(Int, Int)] -> Matrix -> [Tile]
lookupTiles tiles matrix = catMaybes $ map (\(x',y') -> M.lookup x' y' matrix) tiles


advanceTile2 :: Tile -> [Tile] -> Tile
advanceTile2 tile adjacents =
  let adjacentsEmpty    = filter (== EmptySeat) adjacents
      adjacentsOccupied = filter (== OccupiedSeat) adjacents
  in
  case tile of
    OccupiedSeat -> if (length adjacentsOccupied) >= 5 then EmptySeat else OccupiedSeat
    EmptySeat    -> if (null adjacentsOccupied)        then OccupiedSeat else EmptySeat
    Floor        -> Floor

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/11.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp
  let answer2 = solvePart2 input             -- *Year2020.Day11> main
  putStrLn $ "Example 1: " <> show ex1       -- Example 1: 37
  putStrLn $ "   Part 1: " <> show answer1   --    Part 1: 2126 -- (14.61 secs, 5,112,938,080 bytes)
  putStrLn $ "Example 2: " <> show ex2       -- Example 2: 26
  putStrLn $ "   Part 2: " <> show answer2   --    Part 2: 1914 -- (20.25 secs, 16,163,914,848 bytes)
 
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

seeEightOccupiedExinp =
  [ ".......#.",
    "...#.....",
    ".#.......",
    ".........",
    "..#L....#",
    "....#....",
    ".........",
    "#........",
    "...#....."
  ]

noOccupiedSeatsEx = [ ".##.##.",
                      "#.#.#.#",
                      "##...##",
                      "...L...",
                      "##...##",
                      "#.#.#.#",
                      ".##.##." ]

onlyOneOccupiedSeatEx = [ ".............",
                          ".L.L.#.#.#.#.",
                          "............." ]

problemEx = ["#.##.##.##",
             "#######.##",
             "#.#.#..#..",
             "####.##.##",
             "#.##.##.##",
             "#.#####.##",
             "..#.#.....",
             "##########",
             "#.######.#",
             "#.#####.##"]

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
