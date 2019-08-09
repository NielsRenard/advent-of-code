{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day08
  ()
where

import           Prelude                        ( head
                                                , last
                                                , read
                                                , (!!)
                                                , putStrLn
                                                )
import           Data.Char (isDigit, digitToInt)
import           RIO
import qualified RIO.List                      as L
import qualified RIO.List.Partial              as L'
import qualified RIO.Set                       as Set
import           Text.Pretty.Simple             ( pPrint )
import           Text.ParserCombinators.ReadP

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

{- Example

  skip ahead:
  λ print $ rotateColumn 1 1 $ rotateRow 0 4 $ rotateColumn 1 1 $ rect 3 2 $ initScreen 7 3

  step by step:
  λ s0 = initScreen 7 3
  λ print s0
  .......
  .......
  .......

  λ s1 = rect 3 2 s0
  λ print s1
  ###....
  ###....
  .......

  λ s2 = rotateColumn 1 1 s1
  λ print s2
  #.#....
  ###....
  .#.....

  λ s3 = rotateRow 0 4 s2
  λ print s3
  ....#.#
  ###....
  .#.....

  λ s4 = rotateColumn 1 1 s3
  λ print s4
  .#..#.#
  #.#....
  .#.....

-}

-- how many pixels should be lit?
answerOne = L.length $ L.filter (== '#') $ render $ solvePartOne 50 6 allOperations

solvePartOne screenWidth screenHeight operations =
  let screen = initScreen screenWidth screenHeight
  in
    L.foldl' (\s x -> case x of
                       (Rect w h) -> rect w h s
                       (RotateColumn c o) -> rotateColumn c o s
                       (RotateRow r o') -> rotateRow r o' s) screen operations

{-
  ####..##...##..###...##..###..#..#.#...#.##...##..
  #....#..#.#..#.#..#.#..#.#..#.#..#.#...##..#.#..#.
  ###..#..#.#..#.#..#.#....#..#.####..#.#.#..#.#..#.
  #....#..#.####.###..#.##.###..#..#...#..####.#..#.
  #....#..#.#..#.#.#..#..#.#....#..#...#..#..#.#..#.
  ####..##..#..#.#..#..###.#....#..#...#..#..#..##..
-}



data Operation = Rect Width Height | RotateColumn Int Int | RotateRow Int Int deriving (Show)

exampleOperations = L.map (fst . last . readP_to_S operationParser) exampleInput
allOperations = L.map (fst . last . readP_to_S operationParser) input

operationParser :: ReadP Operation
operationParser = do
  op <- string "rect " <|> string "rotate column x=" <|> string "rotate row y="
  case op of
    "rect " -> do
      w <- count 2 digit <|> count 1 digit
      x <- char 'x'
      h <- digitToInt <$> digit
      return $ Rect (read w) h
    "rotate column x=" -> do
      c <- count 2 digit <|> count 1 digit
      by <- string " by "
      offset <- count 2 digit <|> count 1 digit
      return $ RotateColumn (read c) (read offset)
    "rotate row y=" -> do
      r <- count 2 digit <|> count 1 digit
      by <- string " by "
      offset <- count 2 digit <|> count 1 digit
      return $ RotateRow (read r) (read offset)

-- parser of digits
digit :: ReadP Char
digit = satisfy isDigit

rect :: Width -> Height -> Screen -> Screen
rect w h oldScreen =
  let oldScreen' = Set.fromList oldScreen
      newRect    = Set.fromList $ createRect w h
  in  Set.toList $ Set.union newRect oldScreen'

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn x by scr =
  let oldCol = getColumn scr x
      newCol = Set.fromList $ rotateColumn' oldCol by
      scr' = Set.fromList scr
  in Set.toList $
     Set.union newCol scr'

-- duplicate first, generify later
rotateRow :: Int -> Int -> Screen -> Screen
rotateRow x by scr =
  let oldRow = getRow scr x
      newRow = Set.fromList $ rotateRow' oldRow by
      scr' = Set.fromList scr
  in Set.toList $
     Set.union newRow scr'

-- duplicate first, generify later
rotateRow' :: [Pixel] -> Int -> [Pixel]
rotateRow' ps n =
  let
    length = L.length  ps
  in L.map (\p ->
               let currentX = x p
                   nextX = (x p + n)
               in Pixel { lit = lit p
                        , x = if nextX >= length
                              then nextX `mod` length
                              else nextX
                        , y = y p}) ps


rotateColumn' :: [Pixel] -> Int -> [Pixel]
rotateColumn' ps n =
  let
    length = L.length  ps
  in L.map (\p ->
               let currentY = y p
                   nextY = (y p + n)
               in Pixel { lit = lit p
                        , x = x p
                        , y = if nextY >= length
                              then nextY `mod` length
                              else nextY}) ps


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
getColumn :: [Pixel] -> Int -> [Pixel]
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
c1 = getColumn scr0 0
c2 = getColumn scr0 2
r1 = getRow scr0 1
r2 = getRow scr0 2
p0 = Pixel { lit = False, x = 0, y = 0 }
p1 = Pixel { lit = True, x = 0, y = 0 }

-- puzzle input

exampleInput =
  [ "rect 3x2"
  , "rotate column x=1 by 1"
  , "rotate row y=0 by 4"
  , "rotate column x=1 by 1"
  ]

input =
  [
    "rect 1x1"
  , "rotate row y=0 by 7"
  , "rect 1x1"
  , "rotate row y=0 by 5"
  , "rect 1x1"
  , "rotate row y=0 by 5"
  , "rect 1x1"
  , "rotate row y=0 by 2"
  , "rect 1x1"
  , "rotate row y=0 by 3"
  , "rect 1x1"
  , "rotate row y=0 by 5"
  , "rect 1x1"
  , "rotate row y=0 by 3"
  , "rect 1x1"
  , "rotate row y=0 by 2"
  , "rect 1x1"
  , "rotate row y=0 by 3"
  , "rect 2x1"
  , "rotate row y=0 by 7"
  , "rect 6x1"
  , "rotate row y=0 by 3"
  , "rect 2x1"
  , "rotate row y=0 by 2"
  , "rect 1x2"
  , "rotate row y=1 by 10"
  , "rotate row y=0 by 3"
  , "rotate column x=0 by 1"
  , "rect 2x1"
  , "rotate column x=20 by 1"
  , "rotate column x=15 by 1"
  , "rotate column x=5 by 1"
  , "rotate row y=1 by 5"
  , "rotate row y=0 by 2"
  , "rect 1x2"
  , "rotate row y=0 by 5"
  , "rotate column x=0 by 1"
  , "rect 4x1"
  , "rotate row y=2 by 15"
  , "rotate row y=0 by 5"
  , "rotate column x=0 by 1"
  , "rect 4x1"
  , "rotate row y=2 by 5"
  , "rotate row y=0 by 5"
  , "rotate column x=0 by 1"
  , "rect 4x1"
  , "rotate row y=2 by 10"
  , "rotate row y=0 by 10"
  , "rotate column x=8 by 1"
  , "rotate column x=5 by 1"
  , "rotate column x=0 by 1"
  , "rect 9x1"
  , "rotate column x=27 by 1"
  , "rotate row y=0 by 5"
  , "rotate column x=0 by 1"
  , "rect 4x1"
  , "rotate column x=42 by 1"
  , "rotate column x=40 by 1"
  , "rotate column x=22 by 1"
  , "rotate column x=17 by 1"
  , "rotate column x=12 by 1"
  , "rotate column x=7 by 1"
  , "rotate column x=2 by 1"
  , "rotate row y=3 by 10"
  , "rotate row y=2 by 5"
  , "rotate row y=1 by 3"
  , "rotate row y=0 by 10"
  , "rect 1x4"
  , "rotate column x=37 by 2"
  , "rotate row y=3 by 18"
  , "rotate row y=2 by 30"
  , "rotate row y=1 by 7"
  , "rotate row y=0 by 2"
  , "rotate column x=13 by 3"
  , "rotate column x=12 by 1"
  , "rotate column x=10 by 1"
  , "rotate column x=7 by 1"
  , "rotate column x=6 by 3"
  , "rotate column x=5 by 1"
  , "rotate column x=3 by 3"
  , "rotate column x=2 by 1"
  , "rotate column x=0 by 1"
  , "rect 14x1"
  , "rotate column x=38 by 3"
  , "rotate row y=3 by 12"
  , "rotate row y=2 by 10"
  , "rotate row y=0 by 10"
  , "rotate column x=7 by 1"
  , "rotate column x=5 by 1"
  , "rotate column x=2 by 1"
  , "rotate column x=0 by 1"
  , "rect 9x1"
  , "rotate row y=4 by 20"
  , "rotate row y=3 by 25"
  , "rotate row y=2 by 10"
  , "rotate row y=0 by 15"
  , "rotate column x=12 by 1"
  , "rotate column x=10 by 1"
  , "rotate column x=8 by 3"
  , "rotate column x=7 by 1"
  , "rotate column x=5 by 1"
  , "rotate column x=3 by 3"
  , "rotate column x=2 by 1"
  , "rotate column x=0 by 1"
  , "rect 14x1"
  , "rotate column x=34 by 1"
  , "rotate row y=1 by 45"
  , "rotate column x=47 by 1"
  , "rotate column x=42 by 1"
  , "rotate column x=19 by 1"
  , "rotate column x=9 by 2"
  , "rotate row y=4 by 7"
  , "rotate row y=3 by 20"
  , "rotate row y=0 by 7"
  , "rotate column x=5 by 1"
  , "rotate column x=3 by 1"
  , "rotate column x=2 by 1"
  , "rotate column x=0 by 1"
  , "rect 6x1"
  , "rotate row y=4 by 8"
  , "rotate row y=3 by 5"
  , "rotate row y=1 by 5"
  , "rotate column x=5 by 1"
  , "rotate column x=4 by 1"
  , "rotate column x=3 by 2"
  , "rotate column x=2 by 1"
  , "rotate column x=1 by 3"
  , "rotate column x=0 by 1"
  , "rect 6x1"
  , "rotate column x=36 by 3"
  , "rotate column x=25 by 3"
  , "rotate column x=18 by 3"
  , "rotate column x=11 by 3"
  , "rotate column x=3 by 4"
  , "rotate row y=4 by 5"
  , "rotate row y=3 by 5"
  , "rotate row y=2 by 8"
  , "rotate row y=1 by 8"
  , "rotate row y=0 by 3"
  , "rotate column x=3 by 4"
  , "rotate column x=0 by 4"
  , "rect 4x4"
  , "rotate row y=4 by 10"
  , "rotate row y=3 by 20"
  , "rotate row y=1 by 10"
  , "rotate row y=0 by 10"
  , "rotate column x=8 by 1"
  , "rotate column x=7 by 1"
  , "rotate column x=6 by 1"
  , "rotate column x=5 by 1"
  , "rotate column x=3 by 1"
  , "rotate column x=2 by 1"
  , "rotate column x=1 by 1"
  , "rotate column x=0 by 1"
  , "rect 9x1"
  , "rotate row y=0 by 40"
  , "rotate column x=44 by 1"
  , "rotate column x=35 by 5"
  , "rotate column x=18 by 5"
  , "rotate column x=15 by 3"
  , "rotate column x=10 by 5"
  , "rotate row y=5 by 15"
  , "rotate row y=4 by 10"
  , "rotate row y=3 by 40"
  , "rotate row y=2 by 20"
  , "rotate row y=1 by 45"
  , "rotate row y=0 by 35"
  , "rotate column x=48 by 1"
  , "rotate column x=47 by 5"
  , "rotate column x=46 by 5"
  , "rotate column x=45 by 1"
  , "rotate column x=43 by 1"
  , "rotate column x=40 by 1"
  , "rotate column x=38 by 2"
  , "rotate column x=37 by 3"
  , "rotate column x=36 by 2"
  , "rotate column x=32 by 2"
  , "rotate column x=31 by 2"
  , "rotate column x=28 by 1"
  , "rotate column x=23 by 3"
  , "rotate column x=22 by 3"
  , "rotate column x=21 by 5"
  , "rotate column x=20 by 1"
  , "rotate column x=18 by 1"
  , "rotate column x=17 by 3"
  , "rotate column x=13 by 1"
  , "rotate column x=10 by 1"
  , "rotate column x=8 by 1"
  , "rotate column x=7 by 5"
  , "rotate column x=6 by 5"
  , "rotate column x=5 by 1"
  , "rotate column x=3 by 5"
  , "rotate column x=2 by 5"
  , "rotate column x=1 by 5"
  ]
