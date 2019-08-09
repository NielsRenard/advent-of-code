{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day08
  ()
where

import           Prelude                        ( head
                                                , (!!)
                                                , putStrLn
                                                )
import           RIO
import qualified RIO.List                      as L
import qualified RIO.List.Partial              as L'

data Pixel = Pixel { lit :: Bool, x :: Int, y :: Int } deriving (Show)
type Screen = [Pixel]
type Width = Int
type Height = Int

initScreen :: Width -> Height -> Screen
initScreen w h = [ Pixel False x' y' | x' <- [0 .. w], y' <- [0 .. h] ]

renderScreen :: Screen -> String
renderScreen s =
  let maxHeight = y $ L'.maximumBy (comparing y) s
      allRows   = [ getRow s y' | y' <- [0 .. maxHeight] ]
  in  unlines $ L.map (L.map renderPixel) allRows

printScreen :: Screen -> IO ()
printScreen = putStrLn . renderScreen

getRow pxs rowNum = L.filter (\p -> y p == rowNum) pxs

renderPixel :: Pixel -> Char
renderPixel p = if lit p then '#' else '.'

{-
      .##.#
      .##.#
      ###..
-}
scr0 :: Screen
scr0 =
  [ Pixel False 0 0 , Pixel True  1 0 , Pixel True  2 0, Pixel False  3 0, Pixel True  4 0
  , Pixel False 0 1 , Pixel True  1 1 , Pixel True  2 1, Pixel False  3 1, Pixel True  4 1
  , Pixel True 0 2 , Pixel True  1 2 , Pixel True  2 2, Pixel False  3 2, Pixel False  4 2
  ]

p0 = Pixel { lit = False, x = 0, y = 0 }
p1 = Pixel { lit = True, x = 0, y = 0 }
