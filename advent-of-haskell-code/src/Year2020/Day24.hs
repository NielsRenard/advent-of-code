{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Year2020.Day24
  (
  )
where

import Data.Char
import Data.Ord (comparing)
import Data.List as L
import Data.List.Utils
import Data.List.Split as Split hiding (sepBy)
import qualified Data.Set as S
import Data.List.Index
import Data.Maybe
import Data.Text as T (unpack)
import NeatInterpolation (text)
import Debug.Trace (trace)
import System.Directory
import Data.Void
import qualified Data.HashMap.Strict as HM
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

{- parsers -}

type X = Int
type Y = Int
data Colour = Black | White deriving (Show, Eq)
type Tile = ((X, Y), Colour)

data Direction = N | W | E | S | NW | NE | SW | SE deriving (Show, Eq)

directionParser :: Parser [Direction]
directionParser = do
  directionStrings <-  many $ try (string "nw" <|> "ne" <|> "se" <|> "sw" <|>  "n" <|> "w" <|> "e" <|> "s")
  let directions = map f directionStrings
  return $ directions
    where
      f x = case x of
        "nw" -> NW
        "ne" -> NE
        "sw" -> SW
        "se" -> SE
        "n"  -> N
        "w"  -> W
        "e"  -> E
        "s"  -> S

parseInput input =
  catMaybes $ map (parseMaybe directionParser) $ lines input

{- part 1 -}

solvePart1 :: String -> Int
solvePart1 input =
   let directions      = parseInput input
       allEndPositions = map (followDirections (0,0)) directions
       howManyFlips    = frequencies allEndPositions
   in
     length $ filter (odd . snd) howManyFlips

followDirections :: (X,Y) -> [Direction] -> (X,Y)
followDirections pos [] = pos
followDirections pos dir@(x:xs) =
  followDirections (move pos x) xs

move :: (X,Y) -> Direction -> (X, Y)
move pos@(x,y) dir =
  case dir of
    NW -> ( x - 1 , y + 1 )
    N  -> ( x     , y + 1 )
    NE -> ( x     , y + 1 )
    W  -> ( x - 1 , y     )
    E  -> ( x + 1 , y     )
    SW -> ( x     , y - 1 )
    S  -> ( x     , y - 1 )
    SE -> ( x + 1 , y - 1 )

{- part 2 -}

solvePart2 :: String -> [Tile]
solvePart2 input =
   let directions      = parseInput input
       allEndPositions = map (followDirections (0,0)) directions
       howManyFlips    = frequencies allEndPositions
       blackTiles      = filter (odd . snd) howManyFlips 
   in
     map (\it -> (fst it, Black)) blackTiles

--getAdjacent :: [Tile] -> (X,Y) -> [Tile]
--getAdjacent tiles pos =
--  let n = find (x,y) tiles
--  in
--    [n]
  
main = do
  input <- readFile "data/2020/24.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp
--  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2

-- 278659341 too high

{-- Test and example input --}

exinp :: String
exinp = T.unpack $
  [text|
       sesenwnenenewseeswwswswwnenewsewsw
       neeenesenwnwwswnenewnwwsewnenwseswesw
       seswneswswsenwwnwse
       nwnwneseeswswnenewneswwnewseswneseene
       swweswneswnenwsewnwneneseenw
       eesenwseswswnenwswnwnwsewwnwsene
       sewnenenenesenwsewnenwwwse
       wenwwweseeeweswwwnwwe
       wsweesenenewnwwnwsenewsenwwsesesenwne
       neeswseenwwswnwswswnw
       nenwswwsewswnenenewsenwsenwnesesenew
       enewnwewneswsewnwswenweswnenwsenwsw
       sweneswneswneneenwnewenewwneswswnese
       swwesenesewenwneswnwwneseswwne
       enesenwswwswneneswsenwnewswseenwsese
       wnwnesenesenenwwnenwsewesewsesesew
       nenewswnwewswnenesenwnesewesw
       eneswnwswnwsenenwnwnwwseeswneewsenese
       neswnwewnwnwseenwseesewsenwsweewe
       wseweeenwnesenwwwswnew
       |]
