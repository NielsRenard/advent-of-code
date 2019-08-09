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
import qualified RIO.Set                       as Set
import           Text.Pretty.Simple             ( pPrint )

{- TODO: refactor Screen to be a Set. I got tangled up changing from List to Set halfway -}

data Pixel = Pixel { lit :: Bool, x :: Int, y :: Int } deriving (Show)

instance Eq Pixel where
  p1 == p2 = x p1 == x p2 && y p1 == y p2

-- custom comparator that ignores if pixel is 'lit'
instance Ord Pixel where
  compare (Pixel _ x1 y1) (Pixel _ x2 y2) = if xComparison == EQ
    then yComparison
    else xComparison
   where
    xComparison = compare x1 x2
    yComparison = compare y1 y2

type Screen = [Pixel]
type Width = Int
type Height = Int

-- try out the puzzle example:
-- λ rect 3 2 (initScreen 7 3)
rect :: Width -> Height -> Screen -> Screen
rect w h oldScreen =
  let oldScreen' = Set.fromList oldScreen
      newRect    = Set.fromList $ createRect 3 2
  in  Set.toList $ Set.union newRect oldScreen'

initScreen :: Width -> Height -> Screen
initScreen w h = [ Pixel False x' y' | x' <- [0 .. w - 1], y' <- [0 .. h - 1] ]

createRect :: Width -> Height -> Screen
createRect w h = [ Pixel True x' y' | x' <- [0 .. w - 1], y' <- [0 .. h - 1] ]

-- renders an unordered list of pixels of arbitrary size
render :: [Pixel] -> String
render s =
  let maxHeight = y $ L'.maximumBy (comparing y) s
      allRows   = [ getRow s y' | y' <- [0 .. maxHeight] ]
  in  unlines $ L.map (L.map renderPixel) (L.map (L.sortOn x) allRows)

print :: Screen -> IO ()
print = putStrLn . render

getRow :: [Pixel] -> Int -> [Pixel]
getRow pxs rowNum = L.filter (\p -> y p == rowNum) pxs
getColumn pxs colNum = L.filter (\p -> x p == colNum) pxs

renderPixel :: Pixel -> Char
renderPixel p = if lit p then '#' else '.'

-- some example values below

{-
      .##.#
      .##.#
      ###..
-}
scr0 :: Screen
scr0 =
  [ Pixel False 0 0 , Pixel True  1 0 , Pixel True  2 0 , Pixel False 3 0 , Pixel True  4 0
  , Pixel False 0 1 , Pixel True  1 1 , Pixel True  2 1 , Pixel False 3 1 , Pixel True  4 1
  , Pixel True  0 2 , Pixel True  1 2 , Pixel True  2 2 , Pixel False 3 2 , Pixel False 4 2
  ]

p0 = Pixel { lit = False, x = 0, y = 0 }
p1 = Pixel { lit = True, x = 0, y = 0 }
