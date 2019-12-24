{-# LANGUAGE OverloadedStrings #-}

module Year2019.Day24 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text as T
import Utils


type Grid = [Text]
type Tile = Char
type X = Int
type Y = Int

slurp :: Grid -> Set Grid -> Grid
slurp grid acc =
  let grid' = tick grid
  in
    if Set.member grid' acc
    then grid'
    else slurp grid' (Set.insert grid' acc)

tick :: Grid -> Grid
tick g =
  T.chunksOf 5 $ T.pack [transformTile (x,y) g | y <- [0..4], x <- [0..4]]

transformTile :: (X,Y) -> Grid -> Tile
transformTile (x,y) g =
  let adjacent = getAdjacent (x,y) g
      adjacentBugCount =  L.length (L.filter (== '#') adjacent)
  in
  case getTile (x,y) g of
    '#' -> 
      if adjacentBugCount == 1 then '#' else '.'
    '.' ->
      if adjacentBugCount `elem` [1,2] then '#' else '.'

getAdjacent :: (X,Y) -> Grid -> [Tile]
getAdjacent (x,y) g =
  let
    down = getTile (x,y+1) g
    left = getTile (x-1,y) g
    right = getTile (x+1,y) g
    up = getTile (x,y-1) g
  in [up, left, right, down]
  
getTile :: (X,Y) -> Grid -> Tile
getTile (x,y) g = if x `elem` [0..4] && y `elem` [0..4]
                  then T.index (g !! y) x
                  else '.'


invertChar :: Char -> Char
invertChar '#' = '.'
invertChar '.' = '#'

print' :: [Text] -> IO()
print' = mapM_ print


exIn1 :: [Text]
exIn1 =
  [ "....#",
    "#..#.",
    "#..##",
    "..#..",
    "#...."
  ]

input :: [Text]
input =
  ["####.",
   ".###.",
   ".#..#",
   "##.##",
   "###.."]
