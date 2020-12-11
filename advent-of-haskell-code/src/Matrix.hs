{-# LANGUAGE NoImplicitPrelude #-}

module Matrix
  ( Matrix.lookup,
    adjacent4,
    adjacent8    
  )
where

import Data.Maybe
import Data.Char
import Data.List ((!!))
import qualified RIO.Map as M
import qualified RIO.Text as T
import Prelude (Int, (+), (-), ($), length, head, (<), (>), (||), (>=), otherwise)

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
