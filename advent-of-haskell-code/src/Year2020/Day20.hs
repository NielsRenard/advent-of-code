{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Year2020.Day20
  (
  )
where

import Data.List as L
import Data.List.Split as Split
import Data.Set as S hiding (drop, map)
import Data.List.Index
import Data.Maybe
import Data.Text as T (unpack)
import NeatInterpolation (text)
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

parseInput input =
   map labelAndContents $ Split.splitWhen (== "") $ lines input

solvePart1 input =
  let tiles = parseInput input
      allEdges = map getEdges tiles
  in
    product $ map snd $ L.filter ((== "corner piece") . fst) $ map (\tile ->
           case length $ countMatchingEdges tile allEdges of
             2 -> ("corner piece", fst tile)
             3 -> ("edge piece", fst tile)
             4 -> ("inside piece", fst tile)
             ) allEdges

countMatchingEdges :: (Int, [String]) -> [(Int, [String])] -> [S.Set String]
countMatchingEdges (label,edges) allEdges =
  let reversedEdges = map reverse edges
  in
    L.filter (not . (== S.empty)) $
    map (\(label', edge) ->
           if label == label' -- don't match self
           then S.empty
           else (S.fromList (concat [edges, reversedEdges])) `S.intersection` (S.fromList edge))
    allEdges


getEdges :: (Int, [String]) -> (Int, [String])
getEdges (label,tile) = (label, [head tile
                                ,last tile
                                ,(map head tile)
                                ,(map last tile)])

labelAndContents :: [String] -> (Int, [String])
labelAndContents labeledTile =
  let label = (read :: String -> Int) $ reverse $ drop 1 $ reverse $ last $ words $ (head labeledTile)
  in
    (label, tail labeledTile)

main :: IO ()
main = do
  input <- readFile "data/2020/20.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
--  let ex2 = solvePart2 exinp2
--  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: String
exinp = T.unpack $
  [text|
       Tile 2311:
       ..##.#..#.
       ##..#.....
       #...##..#.
       ####.#...#
       ##.##.###.
       ##...#.###
       .#.#.#..##
       ..#....#..
       ###...#.#.
       ..###..###

       Tile 1951:
       #.##...##.
       #.####...#
       .....#..##
       #...######
       .##.#....#
       .###.#####
       ###.##.##.
       .###....#.
       ..#.#..#.#
       #...##.#..

       Tile 1171:
       ####...##.
       #..##.#..#
       ##.#..#.#.
       .###.####.
       ..###.####
       .##....##.
       .#...####.
       #.##.####.
       ####..#...
       .....##...

       Tile 1427:
       ###.##.#..
       .#..#.##..
       .#.##.#..#
       #.#.#.##.#
       ....#...##
       ...##..##.
       ...#.#####
       .#.####.#.
       ..#..###.#
       ..##.#..#.

       Tile 1489:
       ##.#.#....
       ..##...#..
       .##..##...
       ..#...#...
       #####...#.
       #..#.#.#.#
       ...#.#.#..
       ##.#...##.
       ..##.##.##
       ###.##.#..

       Tile 2473:
       #....####.
       #..#.##...
       #.##..#...
       ######.#.#
       .#...#.#.#
       .#########
       .###.#..#.
       ########.#
       ##...##.#.
       ..###.#.#.

       Tile 2971:
       ..#.#....#
       #...###...
       #.#.###...
       ##.##..#..
       .#####..##
       .#..####.#
       #..#.#..#.
       ..####.###
       ..#.#.###.
       ...#.#.#.#

       Tile 2729:
       ...#.#.#.#
       ####.#....
       ..#.#.....
       ....#..#.#
       .##..##.#.
       .#.####...
       ####.#.#..
       ##.####...
       ##..#.##..
       #.##...##.

       Tile 3079:
       #.#.#####.
       .#..######
       ..#.......
       ######....
       ####.#..#.
       .#...#.##.
       #.#####.##
       ..#.###...
       ..#.......
       ..#.###...
       |]
