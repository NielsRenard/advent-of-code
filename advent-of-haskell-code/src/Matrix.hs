{-# LANGUAGE NoImplicitPrelude #-}

module Matrix
  ( Matrix.lookup,
    lookup3d,
    adjacent4,
    adjacent8,
    lineOfSightNorthWest,
    lineOfSightNorth,
    lineOfSightNorthEast,
    lineOfSightWest,
    lineOfSightEast,
    lineOfSightSouthWest,
    lineOfSightSouth,
    lineOfSightSouthEast,
  )
where

import Data.Maybe
import Data.Char
import Data.List ((!!), zip, reverse)
import qualified RIO.Map as M
import qualified RIO.Text as T
import Prelude (Int, (+), (-), ($), length, head, (<), (>), (||), (>=), otherwise, repeat, succ, pred)

{- 3d three dimensional matrix operations -}

lookup3d :: Int -> Int -> Int -> [[[b]]] -> Maybe b
lookup3d x y z matrix
    | (x < 0 || x >= width)  = Nothing
    | (y < 0 || y >= height) = Nothing
    | (z < 0 || z >= depth) = Nothing    
    | otherwise = Just (matrix !! z !! y !! x)
  where
    height = length matrix
    width  = length $ head matrix
    depth  = length $ head matrix    



{- 2d two dimensional matrix operations -}

lookup :: Int -> Int -> [[b]] -> Maybe b
lookup x y matrix
    | (x < 0 || x >= width)  = Nothing
    | (y < 0 || y >= height) = Nothing
    | otherwise = Just (matrix !! y !! x)
  where
    height = length matrix
    width  = length $ head matrix

--        [n]
--   [w]  [x]  [e]
--        [s]
adjacent4 :: Int -> Int -> [[a]] -> [a]
adjacent4 x y matrix =
  let
    n = lookup   x     ( y - 1) matrix
    w = lookup ( x - 1)  y      matrix
    e = lookup ( x + 1)  y      matrix
    s = lookup   x     ( y + 1) matrix
   in
    catMaybes $ [n, w, e, s]

--   [nw] [n] [ne]
--   [w]  [x]  [e]
--   [sw] [s] [se]
adjacent8 :: Int -> Int -> [[a]] -> [a]
adjacent8 x y matrix =
  let
    nw = lookup ( x - 1 ) ( y - 1 ) matrix
    n  = lookup   x       ( y - 1 ) matrix
    ne = lookup ( x + 1 ) ( y - 1 ) matrix
    w  = lookup ( x - 1 )   y       matrix
    e  = lookup ( x + 1 )   y       matrix
    sw = lookup ( x - 1 ) ( y + 1 ) matrix
    s  = lookup   x       ( y + 1 ) matrix
    se = lookup ( x + 1 ) ( y + 1 ) matrix
   in
    catMaybes [nw, n, ne, w, e, sw, s, se]

lineOfSightWest :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightWest x y matrix =
  let width  = length $ head matrix
      west   = reverse $ zip [0..pred x] (repeat y)
  in
    west

lineOfSightEast :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightEast x y matrix =
  let width  = length $ head matrix
      east   = zip [succ x..width] (repeat y)
  in
    east

lineOfSightNorth :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightNorth x y matrix =
  let height = length matrix
      north  = reverse $ zip (repeat x) [0..pred y]
  in
    north

lineOfSightSouth :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightSouth x y matrix =
  let height = length matrix
      south  = zip (repeat x) [succ y..height]
  in
    south

lineOfSightNorthWest :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightNorthWest x y matrix =
  let height = length matrix
      width  = length $ head matrix
      northWest  = zip [(x-1),(x-2)..0] [(y-1),(y-2)..0]
  in
    northWest


lineOfSightNorthEast :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightNorthEast x y matrix =
  let height    = length matrix
      width     =  length $ head matrix
      northEast = zip [succ x..width] [(y-1), (y-2)..0]
  in
    northEast

lineOfSightSouthWest :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightSouthWest x y matrix =
  let height     = length matrix
      width      = length $ head matrix
      southWest  = zip [(x-1), (x-2)..0] [succ y..height]
  in
    southWest

lineOfSightSouthEast :: Int -> Int -> [[a]] -> [(Int, Int)]
lineOfSightSouthEast x y matrix =
  let height = length matrix
      width  = length $ head matrix
      southWest  = zip [succ x..width] [succ y..height]
  in
    southWest
