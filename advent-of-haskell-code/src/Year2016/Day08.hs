{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day08
  ()
where

import qualified Data.List.Split               as Split
import           Data.Text.Lazy.Encoding       as Enc
import           Prelude                        ( head
                                                , read
                                                , (!!)
                                                )
import           RIO
import           RIO.Char                       ( isDigit )
import qualified RIO.List                      as L
import qualified RIO.List.Partial              as L'
import qualified RIO.Map                       as M
import qualified RIO.Set                       as S
import qualified RIO.Text.Lazy                 as TL
import           Text.Pretty.Simple (pPrint)

data Pixel = Pixel { lit :: Bool, x :: Int, y :: Int } deriving (Show)
type Screen = [Pixel]
type Width = Int
type Height = Int

initScreen :: Width -> Height -> Screen
initScreen w h = [Pixel False x' y' | x' <- [0..w] , y' <- [0..h]]

renderScreen :: Screen -> String
renderScreen s = let maxHeight = y $ L'.maximumBy (comparing y) s
                     allRows = [getRow s y' | y' <- [0..maxHeight]]
                 in unlines $ L.map (L.map renderPixel) allRows


getRow pxs rowNum = L.filter (\p -> y p == rowNum ) pxs

renderPixel :: Pixel -> Char
renderPixel p = if lit p then '#' else '.'


{-
      .##
      .##
-}
scr0 :: Screen
scr0 = [Pixel False 0 0, Pixel True 1 0, Pixel True 2 0,
        Pixel False 0 1, Pixel True 1 1, Pixel True 2 1]

p0 = Pixel {lit = False, x = 0, y = 0}
p1 = Pixel {lit = True, x = 0, y = 0}
