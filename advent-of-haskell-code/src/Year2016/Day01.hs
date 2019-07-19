{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Year2016.Day01
  ( draw
  )
where

import           Data.Char
import           Data.Maybe
import           Prelude                        ( toEnum
                                                , last
                                                , print
                                                , read
                                                )
import           RIO                     hiding ( many )
import           RIO.List                      as L
import           RIO.List.Partial              as L'
import           RIO.Text                      as T
import           Text.ParserCombinators.ReadP
import           Diagrams.Prelude        hiding ( translate
                                                , direction
                                                , passing
                                                , turn
                                                )
import           Diagrams.Backend.SVG.CmdLine  as Dc

{--
part 1
Wanted to learn how to use Parser Combinators so did that. Still hardcoding the max three digits for the numbers parser. Think I can change that by using many1.

part 2
Read the assignment wrong initially, thinking it was looking for the first coordinate that it *lands* (and turns) on twice.
--}

-- First attempt at visualising.
myPath :: Diagram B
myPath = pts # fromVertices # strokeLine # pad 1.1
 where
   -- put all the coordinates in a list at the end of the file for now
  cs  = allCoords
  pts = L.map (\c -> p2 (toEnum (x c), toEnum (y c))) cs

draw = mainWith myPath

answers :: IO (Int, Int)
answers = do
  input <- readFileUtf8 "data/2016/input/input_2016_01_a.txt"
  let
--    input = T.pack "R1, R1, R1, R1, R1, R1, R1" -- uncomment to test with hardcoded string
      moves         = parseString input
      answerPartOne = solvePartOne initialPosition moves
      answerPartTwo = solvePartTwo initialPosition moves
--  print $ followPath initialPosition moves
  pure (answerPartOne, answerPartTwo)

initialPosition = Position { coordinate = Coordinate { x = 0, y = 0 }
                           , facing     = North
                           , visited    = []
                           }

solvePartOne initPosition moves =
  let endPosition = followPath initPos moves
      distance    = taxicabDiff initPos endPosition
  in  taxicabDiff initPosition endPosition


solvePartTwo initPosition moves =
  let endPosition = followPath initPosition moves
      coordinates = passedMultipleTimes $ visited endPosition
  in  taxicabDiff initPosition $ Position (last coordinates) North []


parseString :: Text -> [Move]
parseString =
  L.map (fst . last . readP_to_S move . T.unpack) . splitCommaAndStrip

followPath :: Position -> [Move] -> Position
followPath = L.foldl translate

initPos = Position { coordinate = Coordinate { x = 0, y = 0 }
                   , facing     = North
                   , visited    = []
                   }

passedMultipleTimes :: [Coordinate] -> [Coordinate]
passedMultipleTimes cols =
  let uniqueCols = nub $ L.reverse cols
      diff       = cols \\ uniqueCols
  in  diff

taxicabDiff :: Position -> Position -> Int
taxicabDiff p1 p2 =
  let x1 = xco p1
      x2 = xco p2
      y1 = yco p1
      y2 = yco p2
  in  cartDiff x1 x2 y1 y2
 where
  xco p = x $ coordinate p
  yco p = y $ coordinate p

cartDiff x1 x2 y1 y2 = abs (x1 - x2) + abs (y1 - y2)

data Move = Move
  { direction :: Turn
  , steps :: Int
  } deriving (Show)

data Turn
  = TurnLeft
  | TurnRight
  deriving (Show)

data Compass
  = North
  | East
  | South
  | West
  deriving (Show)

data Position = Position { coordinate :: Coordinate
                         , facing :: Compass
                         , visited :: [Coordinate]
                         } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving (Show, Eq)

translate :: Position -> Move -> Position
translate position move =
  let x'      = x $ coordinate position
      y'      = y $ coordinate position
      v'      = passing position move ++ visited position
      compass = facing position
      n       = steps move
  in  case direction move of
        TurnLeft -> case compass of
          North -> Position (Coordinate (x' - n) y') West v'
          East  -> Position (Coordinate x' (y' + n)) North v'
          South -> Position (Coordinate (x' + n) y') East v'
          West  -> Position (Coordinate x' (y' - n)) South v'
        TurnRight -> case compass of
          North -> Position (Coordinate (x' + n) y') East v'
          East  -> Position (Coordinate x' (y' - n)) South v'
          South -> Position (Coordinate (x' - n) y') West v'
          West  -> Position (Coordinate x' (y' + n)) North v'

passing :: Position -> Move -> [Coordinate]
passing p m =
  let
    newPos = translate p m
    oldX   = x $ coordinate p
    oldY   = y $ coordinate p
    newX   = x $ coordinate newPos
    newY   = y $ coordinate newPos
    passX  = if oldX < newX then [oldX .. newX] else L.reverse [newX .. oldX]
    passY  = if oldY < newY then [oldY .. newY] else L.reverse [newY .. oldY]
  in
    L'.tail [ Coordinate { x = x', y = y' } | x' <- passX, y' <- passY ]

-- parser of Moves
move :: ReadP Move
move = do
  direction <- turn
  steps     <- numbers
  return $ Move direction steps

-- parser of Turns
turn :: ReadP Turn
turn = do
  t' <- char 'L' <|> char 'R'
  return
    (case t' of
      'L' -> TurnLeft
      'R' -> TurnRight
    )

-- parser of digits
digit :: ReadP Char
digit = satisfy isDigit

-- parser of numbers max 3 digits long
numbers :: ReadP Int
numbers = do
  n <- count 3 digit <|> count 2 digit <|> count 1 digit
  return (read n)

-- cleaning function
splitCommaAndStrip :: Text -> [Text]
splitCommaAndStrip = L.map T.strip . T.split (== ',')

allCoords =
  [ Coordinate { x = -173, y = -156 }
  , Coordinate { x = -173, y = -157 }
  , Coordinate { x = -173, y = -158 }
  , Coordinate { x = -173, y = -159 }
  , Coordinate { x = -171, y = -155 }
  , Coordinate { x = -172, y = -155 }
  , Coordinate { x = -173, y = -155 }
  , Coordinate { x = -170, y = -151 }
  , Coordinate { x = -170, y = -152 }
  , Coordinate { x = -170, y = -153 }
  , Coordinate { x = -170, y = -154 }
  , Coordinate { x = -170, y = -155 }
  , Coordinate { x = -167, y = -150 }
  , Coordinate { x = -168, y = -150 }
  , Coordinate { x = -169, y = -150 }
  , Coordinate { x = -170, y = -150 }
  , Coordinate { x = -166, y = -153 }
  , Coordinate { x = -166, y = -152 }
  , Coordinate { x = -166, y = -151 }
  , Coordinate { x = -166, y = -150 }
  , Coordinate { x = -167, y = -154 }
  , Coordinate { x = -166, y = -154 }
  , Coordinate { x = -168, y = -154 }
  , Coordinate { x = -164, y = -155 }
  , Coordinate { x = -165, y = -155 }
  , Coordinate { x = -166, y = -155 }
  , Coordinate { x = -167, y = -155 }
  , Coordinate { x = -168, y = -155 }
  , Coordinate { x = -163, y = -154 }
  , Coordinate { x = -163, y = -155 }
  , Coordinate { x = -162, y = -153 }
  , Coordinate { x = -163, y = -153 }
  , Coordinate { x = -161, y = -157 }
  , Coordinate { x = -161, y = -156 }
  , Coordinate { x = -161, y = -155 }
  , Coordinate { x = -161, y = -154 }
  , Coordinate { x = -161, y = -153 }
  , Coordinate { x = -158, y = -158 }
  , Coordinate { x = -159, y = -158 }
  , Coordinate { x = -160, y = -158 }
  , Coordinate { x = -161, y = -158 }
  , Coordinate { x = -157, y = -160 }
  , Coordinate { x = -157, y = -159 }
  , Coordinate { x = -157, y = -158 }
  , Coordinate { x = -154, y = -161 }
  , Coordinate { x = -155, y = -161 }
  , Coordinate { x = -156, y = -161 }
  , Coordinate { x = -157, y = -161 }
  , Coordinate { x = -153, y = -161 }
  , Coordinate { x = -152, y = -160 }
  , Coordinate { x = -153, y = -160 }
  , Coordinate { x = -151, y = -158 }
  , Coordinate { x = -151, y = -159 }
  , Coordinate { x = -151, y = -160 }
  , Coordinate { x = -149, y = -157 }
  , Coordinate { x = -150, y = -157 }
  , Coordinate { x = -151, y = -157 }
  , Coordinate { x = -148, y = -157 }
  , Coordinate { x = -152, y = -156 }
  , Coordinate { x = -151, y = -156 }
  , Coordinate { x = -150, y = -156 }
  , Coordinate { x = -149, y = -156 }
  , Coordinate { x = -148, y = -156 }
  , Coordinate { x = -153, y = -155 }
  , Coordinate { x = -153, y = -156 }
  , Coordinate { x = -155, y = -154 }
  , Coordinate { x = -154, y = -154 }
  , Coordinate { x = -153, y = -154 }
  , Coordinate { x = -156, y = -152 }
  , Coordinate { x = -156, y = -153 }
  , Coordinate { x = -156, y = -154 }
  , Coordinate { x = -158, y = -151 }
  , Coordinate { x = -157, y = -151 }
  , Coordinate { x = -156, y = -151 }
  , Coordinate { x = -159, y = -154 }
  , Coordinate { x = -159, y = -153 }
  , Coordinate { x = -159, y = -152 }
  , Coordinate { x = -159, y = -151 }
  , Coordinate { x = -159, y = -155 }
  , Coordinate { x = -160, y = -151 }
  , Coordinate { x = -160, y = -152 }
  , Coordinate { x = -160, y = -153 }
  , Coordinate { x = -160, y = -154 }
  , Coordinate { x = -160, y = -155 }
  , Coordinate { x = -161, y = -150 }
  , Coordinate { x = -160, y = -150 }
  , Coordinate { x = -162, y = -154 }
  , Coordinate { x = -162, y = -153 }
  , Coordinate { x = -162, y = -152 }
  , Coordinate { x = -162, y = -151 }
  , Coordinate { x = -162, y = -150 }
  , Coordinate { x = -164, y = -155 }
  , Coordinate { x = -163, y = -155 }
  , Coordinate { x = -162, y = -155 }
  , Coordinate { x = -165, y = -153 }
  , Coordinate { x = -165, y = -154 }
  , Coordinate { x = -165, y = -155 }
  , Coordinate { x = -165, y = -152 }
  , Coordinate { x = -164, y = -149 }
  , Coordinate { x = -164, y = -150 }
  , Coordinate { x = -164, y = -151 }
  , Coordinate { x = -164, y = -152 }
  , Coordinate { x = -164, y = -148 }
  , Coordinate { x = -165, y = -147 }
  , Coordinate { x = -165, y = -148 }
  , Coordinate { x = -165, y = -146 }
  , Coordinate { x = -166, y = -150 }
  , Coordinate { x = -166, y = -149 }
  , Coordinate { x = -166, y = -148 }
  , Coordinate { x = -166, y = -147 }
  , Coordinate { x = -166, y = -146 }
  , Coordinate { x = -165, y = -151 }
  , Coordinate { x = -166, y = -151 }
  , Coordinate { x = -164, y = -149 }
  , Coordinate { x = -164, y = -150 }
  , Coordinate { x = -164, y = -151 }
  , Coordinate { x = -161, y = -148 }
  , Coordinate { x = -162, y = -148 }
  , Coordinate { x = -163, y = -148 }
  , Coordinate { x = -164, y = -148 }
  , Coordinate { x = -160, y = -145 }
  , Coordinate { x = -160, y = -146 }
  , Coordinate { x = -160, y = -147 }
  , Coordinate { x = -160, y = -148 }
  , Coordinate { x = -160, y = -144 }
  , Coordinate { x = -161, y = -144 }
  , Coordinate { x = -163, y = -143 }
  , Coordinate { x = -162, y = -143 }
  , Coordinate { x = -161, y = -143 }
  , Coordinate { x = -164, y = -139 }
  , Coordinate { x = -164, y = -140 }
  , Coordinate { x = -164, y = -141 }
  , Coordinate { x = -164, y = -142 }
  , Coordinate { x = -164, y = -143 }
  , Coordinate { x = -168, y = -138 }
  , Coordinate { x = -167, y = -138 }
  , Coordinate { x = -166, y = -138 }
  , Coordinate { x = -165, y = -138 }
  , Coordinate { x = -164, y = -138 }
  , Coordinate { x = -169, y = -135 }
  , Coordinate { x = -169, y = -136 }
  , Coordinate { x = -169, y = -137 }
  , Coordinate { x = -169, y = -138 }
  , Coordinate { x = -171, y = -134 }
  , Coordinate { x = -170, y = -134 }
  , Coordinate { x = -169, y = -134 }
  , Coordinate { x = -172, y = -130 }
  , Coordinate { x = -172, y = -131 }
  , Coordinate { x = -172, y = -132 }
  , Coordinate { x = -172, y = -133 }
  , Coordinate { x = -172, y = -134 }
  , Coordinate { x = -168, y = -129 }
  , Coordinate { x = -169, y = -129 }
  , Coordinate { x = -170, y = -129 }
  , Coordinate { x = -171, y = -129 }
  , Coordinate { x = -172, y = -129 }
  , Coordinate { x = -167, y = -128 }
  , Coordinate { x = -167, y = -129 }
  , Coordinate { x = -167, y = -127 }
  , Coordinate { x = -168, y = -129 }
  , Coordinate { x = -168, y = -128 }
  , Coordinate { x = -168, y = -127 }
  , Coordinate { x = -164, y = -130 }
  , Coordinate { x = -165, y = -130 }
  , Coordinate { x = -166, y = -130 }
  , Coordinate { x = -167, y = -130 }
  , Coordinate { x = -168, y = -130 }
  , Coordinate { x = -163, y = -131 }
  , Coordinate { x = -163, y = -130 }
  , Coordinate { x = -162, y = -132 }
  , Coordinate { x = -163, y = -132 }
  , Coordinate { x = -161, y = -129 }
  , Coordinate { x = -161, y = -130 }
  , Coordinate { x = -161, y = -131 }
  , Coordinate { x = -161, y = -132 }
  , Coordinate { x = -158, y = -128 }
  , Coordinate { x = -159, y = -128 }
  , Coordinate { x = -160, y = -128 }
  , Coordinate { x = -161, y = -128 }
  , Coordinate { x = -157, y = -130 }
  , Coordinate { x = -157, y = -129 }
  , Coordinate { x = -157, y = -128 }
  , Coordinate { x = -161, y = -131 }
  , Coordinate { x = -160, y = -131 }
  , Coordinate { x = -159, y = -131 }
  , Coordinate { x = -158, y = -131 }
  , Coordinate { x = -157, y = -131 }
  , Coordinate { x = -162, y = -135 }
  , Coordinate { x = -162, y = -134 }
  , Coordinate { x = -162, y = -133 }
  , Coordinate { x = -162, y = -132 }
  , Coordinate { x = -162, y = -131 }
  , Coordinate { x = -160, y = -136 }
  , Coordinate { x = -161, y = -136 }
  , Coordinate { x = -162, y = -136 }
  , Coordinate { x = -159, y = -139 }
  , Coordinate { x = -159, y = -138 }
  , Coordinate { x = -159, y = -137 }
  , Coordinate { x = -159, y = -136 }
  , Coordinate { x = -159, y = -140 }
  , Coordinate { x = -160, y = -144 }
  , Coordinate { x = -160, y = -143 }
  , Coordinate { x = -160, y = -142 }
  , Coordinate { x = -160, y = -141 }
  , Coordinate { x = -160, y = -140 }
  , Coordinate { x = -160, y = -145 }
  , Coordinate { x = -161, y = -148 }
  , Coordinate { x = -161, y = -147 }
  , Coordinate { x = -161, y = -146 }
  , Coordinate { x = -161, y = -145 }
  , Coordinate { x = -163, y = -149 }
  , Coordinate { x = -162, y = -149 }
  , Coordinate { x = -161, y = -149 }
  , Coordinate { x = -164, y = -145 }
  , Coordinate { x = -164, y = -146 }
  , Coordinate { x = -164, y = -147 }
  , Coordinate { x = -164, y = -148 }
  , Coordinate { x = -164, y = -149 }
  , Coordinate { x = -165, y = -144 }
  , Coordinate { x = -164, y = -144 }
  , Coordinate { x = -166, y = -144 }
  , Coordinate { x = -162, y = -145 }
  , Coordinate { x = -163, y = -145 }
  , Coordinate { x = -164, y = -145 }
  , Coordinate { x = -165, y = -145 }
  , Coordinate { x = -166, y = -145 }
  , Coordinate { x = -161, y = -146 }
  , Coordinate { x = -161, y = -145 }
  , Coordinate { x = -157, y = -147 }
  , Coordinate { x = -158, y = -147 }
  , Coordinate { x = -159, y = -147 }
  , Coordinate { x = -160, y = -147 }
  , Coordinate { x = -161, y = -147 }
  , Coordinate { x = -156, y = -144 }
  , Coordinate { x = -156, y = -145 }
  , Coordinate { x = -156, y = -146 }
  , Coordinate { x = -156, y = -147 }
  , Coordinate { x = -159, y = -143 }
  , Coordinate { x = -158, y = -143 }
  , Coordinate { x = -157, y = -143 }
  , Coordinate { x = -156, y = -143 }
  , Coordinate { x = -160, y = -146 }
  , Coordinate { x = -160, y = -145 }
  , Coordinate { x = -160, y = -144 }
  , Coordinate { x = -160, y = -143 }
  , Coordinate { x = -158, y = -147 }
  , Coordinate { x = -159, y = -147 }
  , Coordinate { x = -160, y = -147 }
  , Coordinate { x = -157, y = -147 }
  , Coordinate { x = -160, y = -146 }
  , Coordinate { x = -159, y = -146 }
  , Coordinate { x = -158, y = -146 }
  , Coordinate { x = -157, y = -146 }
  , Coordinate { x = -161, y = -146 }
  , Coordinate { x = -159, y = -147 }
  , Coordinate { x = -160, y = -147 }
  , Coordinate { x = -161, y = -147 }
  , Coordinate { x = -158, y = -147 }
  , Coordinate { x = -158, y = -148 }
  , Coordinate { x = -159, y = -146 }
  , Coordinate { x = -159, y = -147 }
  , Coordinate { x = -159, y = -148 }
  , Coordinate { x = -160, y = -145 }
  , Coordinate { x = -159, y = -145 }
  , Coordinate { x = -161, y = -149 }
  , Coordinate { x = -161, y = -148 }
  , Coordinate { x = -161, y = -147 }
  , Coordinate { x = -161, y = -146 }
  , Coordinate { x = -161, y = -145 }
  , Coordinate { x = -163, y = -150 }
  , Coordinate { x = -162, y = -150 }
  , Coordinate { x = -161, y = -150 }
  , Coordinate { x = -164, y = -146 }
  , Coordinate { x = -164, y = -147 }
  , Coordinate { x = -164, y = -148 }
  , Coordinate { x = -164, y = -149 }
  , Coordinate { x = -164, y = -150 }
  , Coordinate { x = -168, y = -145 }
  , Coordinate { x = -167, y = -145 }
  , Coordinate { x = -166, y = -145 }
  , Coordinate { x = -165, y = -145 }
  , Coordinate { x = -164, y = -145 }
  , Coordinate { x = -169, y = -148 }
  , Coordinate { x = -169, y = -147 }
  , Coordinate { x = -169, y = -146 }
  , Coordinate { x = -169, y = -145 }
  , Coordinate { x = -168, y = -149 }
  , Coordinate { x = -169, y = -149 }
  , Coordinate { x = -167, y = -147 }
  , Coordinate { x = -167, y = -148 }
  , Coordinate { x = -167, y = -149 }
  , Coordinate { x = -168, y = -146 }
  , Coordinate { x = -167, y = -146 }
  , Coordinate { x = -169, y = -146 }
  , Coordinate { x = -167, y = -145 }
  , Coordinate { x = -168, y = -145 }
  , Coordinate { x = -169, y = -145 }
  , Coordinate { x = -166, y = -145 }
  , Coordinate { x = -163, y = -144 }
  , Coordinate { x = -164, y = -144 }
  , Coordinate { x = -165, y = -144 }
  , Coordinate { x = -166, y = -144 }
  , Coordinate { x = -162, y = -144 }
  , Coordinate { x = -159, y = -145 }
  , Coordinate { x = -160, y = -145 }
  , Coordinate { x = -161, y = -145 }
  , Coordinate { x = -162, y = -145 }
  , Coordinate { x = -158, y = 45 }
  , Coordinate { x = -158, y = 44 }
  , Coordinate { x = -158, y = 43 }
  , Coordinate { x = -158, y = 42 }
  , Coordinate { x = -158, y = 41 }
  , Coordinate { x = -158, y = 40 }
  , Coordinate { x = -158, y = 39 }
  , Coordinate { x = -158, y = 38 }
  , Coordinate { x = -158, y = 37 }
  , Coordinate { x = -158, y = 36 }
  , Coordinate { x = -158, y = 35 }
  , Coordinate { x = -158, y = 34 }
  , Coordinate { x = -158, y = 33 }
  , Coordinate { x = -158, y = 32 }
  , Coordinate { x = -158, y = 31 }
  , Coordinate { x = -158, y = 30 }
  , Coordinate { x = -158, y = 29 }
  , Coordinate { x = -158, y = 28 }
  , Coordinate { x = -158, y = 27 }
  , Coordinate { x = -158, y = 26 }
  , Coordinate { x = -158, y = 25 }
  , Coordinate { x = -158, y = 24 }
  , Coordinate { x = -158, y = 23 }
  , Coordinate { x = -158, y = 22 }
  , Coordinate { x = -158, y = 21 }
  , Coordinate { x = -158, y = 20 }
  , Coordinate { x = -158, y = 19 }
  , Coordinate { x = -158, y = 18 }
  , Coordinate { x = -158, y = 17 }
  , Coordinate { x = -158, y = 16 }
  , Coordinate { x = -158, y = 15 }
  , Coordinate { x = -158, y = 14 }
  , Coordinate { x = -158, y = 13 }
  , Coordinate { x = -158, y = 12 }
  , Coordinate { x = -158, y = 11 }
  , Coordinate { x = -158, y = 10 }
  , Coordinate { x = -158, y = 9 }
  , Coordinate { x = -158, y = 8 }
  , Coordinate { x = -158, y = 7 }
  , Coordinate { x = -158, y = 6 }
  , Coordinate { x = -158, y = 5 }
  , Coordinate { x = -158, y = 4 }
  , Coordinate { x = -158, y = 3 }
  , Coordinate { x = -158, y = 2 }
  , Coordinate { x = -158, y = 1 }
  , Coordinate { x = -158, y = 0 }
  , Coordinate { x = -158, y = -1 }
  , Coordinate { x = -158, y = -2 }
  , Coordinate { x = -158, y = -3 }
  , Coordinate { x = -158, y = -4 }
  , Coordinate { x = -158, y = -5 }
  , Coordinate { x = -158, y = -6 }
  , Coordinate { x = -158, y = -7 }
  , Coordinate { x = -158, y = -8 }
  , Coordinate { x = -158, y = -9 }
  , Coordinate { x = -158, y = -10 }
  , Coordinate { x = -158, y = -11 }
  , Coordinate { x = -158, y = -12 }
  , Coordinate { x = -158, y = -13 }
  , Coordinate { x = -158, y = -14 }
  , Coordinate { x = -158, y = -15 }
  , Coordinate { x = -158, y = -16 }
  , Coordinate { x = -158, y = -17 }
  , Coordinate { x = -158, y = -18 }
  , Coordinate { x = -158, y = -19 }
  , Coordinate { x = -158, y = -20 }
  , Coordinate { x = -158, y = -21 }
  , Coordinate { x = -158, y = -22 }
  , Coordinate { x = -158, y = -23 }
  , Coordinate { x = -158, y = -24 }
  , Coordinate { x = -158, y = -25 }
  , Coordinate { x = -158, y = -26 }
  , Coordinate { x = -158, y = -27 }
  , Coordinate { x = -158, y = -28 }
  , Coordinate { x = -158, y = -29 }
  , Coordinate { x = -158, y = -30 }
  , Coordinate { x = -158, y = -31 }
  , Coordinate { x = -158, y = -32 }
  , Coordinate { x = -158, y = -33 }
  , Coordinate { x = -158, y = -34 }
  , Coordinate { x = -158, y = -35 }
  , Coordinate { x = -158, y = -36 }
  , Coordinate { x = -158, y = -37 }
  , Coordinate { x = -158, y = -38 }
  , Coordinate { x = -158, y = -39 }
  , Coordinate { x = -158, y = -40 }
  , Coordinate { x = -158, y = -41 }
  , Coordinate { x = -158, y = -42 }
  , Coordinate { x = -158, y = -43 }
  , Coordinate { x = -158, y = -44 }
  , Coordinate { x = -158, y = -45 }
  , Coordinate { x = -158, y = -46 }
  , Coordinate { x = -158, y = -47 }
  , Coordinate { x = -158, y = -48 }
  , Coordinate { x = -158, y = -49 }
  , Coordinate { x = -158, y = -50 }
  , Coordinate { x = -158, y = -51 }
  , Coordinate { x = -158, y = -52 }
  , Coordinate { x = -158, y = -53 }
  , Coordinate { x = -158, y = -54 }
  , Coordinate { x = -158, y = -55 }
  , Coordinate { x = -158, y = -56 }
  , Coordinate { x = -158, y = -57 }
  , Coordinate { x = -158, y = -58 }
  , Coordinate { x = -158, y = -59 }
  , Coordinate { x = -158, y = -60 }
  , Coordinate { x = -158, y = -61 }
  , Coordinate { x = -158, y = -62 }
  , Coordinate { x = -158, y = -63 }
  , Coordinate { x = -158, y = -64 }
  , Coordinate { x = -158, y = -65 }
  , Coordinate { x = -158, y = -66 }
  , Coordinate { x = -158, y = -67 }
  , Coordinate { x = -158, y = -68 }
  , Coordinate { x = -158, y = -69 }
  , Coordinate { x = -158, y = -70 }
  , Coordinate { x = -158, y = -71 }
  , Coordinate { x = -158, y = -72 }
  , Coordinate { x = -158, y = -73 }
  , Coordinate { x = -158, y = -74 }
  , Coordinate { x = -158, y = -75 }
  , Coordinate { x = -158, y = -76 }
  , Coordinate { x = -158, y = -77 }
  , Coordinate { x = -158, y = -78 }
  , Coordinate { x = -158, y = -79 }
  , Coordinate { x = -158, y = -80 }
  , Coordinate { x = -158, y = -81 }
  , Coordinate { x = -158, y = -82 }
  , Coordinate { x = -158, y = -83 }
  , Coordinate { x = -158, y = -84 }
  , Coordinate { x = -158, y = -85 }
  , Coordinate { x = -158, y = -86 }
  , Coordinate { x = -158, y = -87 }
  , Coordinate { x = -158, y = -88 }
  , Coordinate { x = -158, y = -89 }
  , Coordinate { x = -158, y = -90 }
  , Coordinate { x = -158, y = -91 }
  , Coordinate { x = -158, y = -92 }
  , Coordinate { x = -158, y = -93 }
  , Coordinate { x = -158, y = -94 }
  , Coordinate { x = -158, y = -95 }
  , Coordinate { x = -158, y = -96 }
  , Coordinate { x = -158, y = -97 }
  , Coordinate { x = -158, y = -98 }
  , Coordinate { x = -158, y = -99 }
  , Coordinate { x = -158, y = -100 }
  , Coordinate { x = -158, y = -101 }
  , Coordinate { x = -158, y = -102 }
  , Coordinate { x = -158, y = -103 }
  , Coordinate { x = -158, y = -104 }
  , Coordinate { x = -158, y = -105 }
  , Coordinate { x = -158, y = -106 }
  , Coordinate { x = -158, y = -107 }
  , Coordinate { x = -158, y = -108 }
  , Coordinate { x = -158, y = -109 }
  , Coordinate { x = -158, y = -110 }
  , Coordinate { x = -158, y = -111 }
  , Coordinate { x = -158, y = -112 }
  , Coordinate { x = -158, y = -113 }
  , Coordinate { x = -158, y = -114 }
  , Coordinate { x = -158, y = -115 }
  , Coordinate { x = -158, y = -116 }
  , Coordinate { x = -158, y = -117 }
  , Coordinate { x = -158, y = -118 }
  , Coordinate { x = -158, y = -119 }
  , Coordinate { x = -158, y = -120 }
  , Coordinate { x = -158, y = -121 }
  , Coordinate { x = -158, y = -122 }
  , Coordinate { x = -158, y = -123 }
  , Coordinate { x = -158, y = -124 }
  , Coordinate { x = -158, y = -125 }
  , Coordinate { x = -158, y = -126 }
  , Coordinate { x = -158, y = -127 }
  , Coordinate { x = -158, y = -128 }
  , Coordinate { x = -158, y = -129 }
  , Coordinate { x = -158, y = -130 }
  , Coordinate { x = -158, y = -131 }
  , Coordinate { x = -158, y = -132 }
  , Coordinate { x = -158, y = -133 }
  , Coordinate { x = -158, y = -134 }
  , Coordinate { x = -158, y = -135 }
  , Coordinate { x = -158, y = -136 }
  , Coordinate { x = -158, y = -137 }
  , Coordinate { x = -158, y = -138 }
  , Coordinate { x = -158, y = -139 }
  , Coordinate { x = -158, y = -140 }
  , Coordinate { x = -158, y = -141 }
  , Coordinate { x = -158, y = -142 }
  , Coordinate { x = -158, y = -143 }
  , Coordinate { x = -158, y = -144 }
  , Coordinate { x = -158, y = -145 }
  , Coordinate { x = -160, y = 46 }
  , Coordinate { x = -159, y = 46 }
  , Coordinate { x = -158, y = 46 }
  , Coordinate { x = -161, y = 48 }
  , Coordinate { x = -161, y = 47 }
  , Coordinate { x = -161, y = 46 }
  , Coordinate { x = -159, y = 49 }
  , Coordinate { x = -160, y = 49 }
  , Coordinate { x = -161, y = 49 }
  , Coordinate { x = -158, y = 51 }
  , Coordinate { x = -158, y = 50 }
  , Coordinate { x = -158, y = 49 }
  , Coordinate { x = -161, y = 52 }
  , Coordinate { x = -160, y = 52 }
  , Coordinate { x = -159, y = 52 }
  , Coordinate { x = -158, y = 52 }
  , Coordinate { x = -162, y = 52 }
  , Coordinate { x = -158, y = 53 }
  , Coordinate { x = -159, y = 53 }
  , Coordinate { x = -160, y = 53 }
  , Coordinate { x = -161, y = 53 }
  , Coordinate { x = -162, y = 53 }
  , Coordinate { x = -157, y = 53 }
  , Coordinate { x = -154, y = 54 }
  , Coordinate { x = -155, y = 54 }
  , Coordinate { x = -156, y = 54 }
  , Coordinate { x = -157, y = 54 }
  , Coordinate { x = -153, y = 56 }
  , Coordinate { x = -153, y = 55 }
  , Coordinate { x = -153, y = 54 }
  , Coordinate { x = -230, y = 57 }
  , Coordinate { x = -229, y = 57 }
  , Coordinate { x = -228, y = 57 }
  , Coordinate { x = -227, y = 57 }
  , Coordinate { x = -226, y = 57 }
  , Coordinate { x = -225, y = 57 }
  , Coordinate { x = -224, y = 57 }
  , Coordinate { x = -223, y = 57 }
  , Coordinate { x = -222, y = 57 }
  , Coordinate { x = -221, y = 57 }
  , Coordinate { x = -220, y = 57 }
  , Coordinate { x = -219, y = 57 }
  , Coordinate { x = -218, y = 57 }
  , Coordinate { x = -217, y = 57 }
  , Coordinate { x = -216, y = 57 }
  , Coordinate { x = -215, y = 57 }
  , Coordinate { x = -214, y = 57 }
  , Coordinate { x = -213, y = 57 }
  , Coordinate { x = -212, y = 57 }
  , Coordinate { x = -211, y = 57 }
  , Coordinate { x = -210, y = 57 }
  , Coordinate { x = -209, y = 57 }
  , Coordinate { x = -208, y = 57 }
  , Coordinate { x = -207, y = 57 }
  , Coordinate { x = -206, y = 57 }
  , Coordinate { x = -205, y = 57 }
  , Coordinate { x = -204, y = 57 }
  , Coordinate { x = -203, y = 57 }
  , Coordinate { x = -202, y = 57 }
  , Coordinate { x = -201, y = 57 }
  , Coordinate { x = -200, y = 57 }
  , Coordinate { x = -199, y = 57 }
  , Coordinate { x = -198, y = 57 }
  , Coordinate { x = -197, y = 57 }
  , Coordinate { x = -196, y = 57 }
  , Coordinate { x = -195, y = 57 }
  , Coordinate { x = -194, y = 57 }
  , Coordinate { x = -193, y = 57 }
  , Coordinate { x = -192, y = 57 }
  , Coordinate { x = -191, y = 57 }
  , Coordinate { x = -190, y = 57 }
  , Coordinate { x = -189, y = 57 }
  , Coordinate { x = -188, y = 57 }
  , Coordinate { x = -187, y = 57 }
  , Coordinate { x = -186, y = 57 }
  , Coordinate { x = -185, y = 57 }
  , Coordinate { x = -184, y = 57 }
  , Coordinate { x = -183, y = 57 }
  , Coordinate { x = -182, y = 57 }
  , Coordinate { x = -181, y = 57 }
  , Coordinate { x = -180, y = 57 }
  , Coordinate { x = -179, y = 57 }
  , Coordinate { x = -178, y = 57 }
  , Coordinate { x = -177, y = 57 }
  , Coordinate { x = -176, y = 57 }
  , Coordinate { x = -175, y = 57 }
  , Coordinate { x = -174, y = 57 }
  , Coordinate { x = -173, y = 57 }
  , Coordinate { x = -172, y = 57 }
  , Coordinate { x = -171, y = 57 }
  , Coordinate { x = -170, y = 57 }
  , Coordinate { x = -169, y = 57 }
  , Coordinate { x = -168, y = 57 }
  , Coordinate { x = -167, y = 57 }
  , Coordinate { x = -166, y = 57 }
  , Coordinate { x = -165, y = 57 }
  , Coordinate { x = -164, y = 57 }
  , Coordinate { x = -163, y = 57 }
  , Coordinate { x = -162, y = 57 }
  , Coordinate { x = -161, y = 57 }
  , Coordinate { x = -160, y = 57 }
  , Coordinate { x = -159, y = 57 }
  , Coordinate { x = -158, y = 57 }
  , Coordinate { x = -157, y = 57 }
  , Coordinate { x = -156, y = 57 }
  , Coordinate { x = -155, y = 57 }
  , Coordinate { x = -154, y = 57 }
  , Coordinate { x = -153, y = 57 }
  , Coordinate { x = -231, y = 57 }
  , Coordinate { x = -231, y = 56 }
  , Coordinate { x = -230, y = 4 }
  , Coordinate { x = -230, y = 5 }
  , Coordinate { x = -230, y = 6 }
  , Coordinate { x = -230, y = 7 }
  , Coordinate { x = -230, y = 8 }
  , Coordinate { x = -230, y = 9 }
  , Coordinate { x = -230, y = 10 }
  , Coordinate { x = -230, y = 11 }
  , Coordinate { x = -230, y = 12 }
  , Coordinate { x = -230, y = 13 }
  , Coordinate { x = -230, y = 14 }
  , Coordinate { x = -230, y = 15 }
  , Coordinate { x = -230, y = 16 }
  , Coordinate { x = -230, y = 17 }
  , Coordinate { x = -230, y = 18 }
  , Coordinate { x = -230, y = 19 }
  , Coordinate { x = -230, y = 20 }
  , Coordinate { x = -230, y = 21 }
  , Coordinate { x = -230, y = 22 }
  , Coordinate { x = -230, y = 23 }
  , Coordinate { x = -230, y = 24 }
  , Coordinate { x = -230, y = 25 }
  , Coordinate { x = -230, y = 26 }
  , Coordinate { x = -230, y = 27 }
  , Coordinate { x = -230, y = 28 }
  , Coordinate { x = -230, y = 29 }
  , Coordinate { x = -230, y = 30 }
  , Coordinate { x = -230, y = 31 }
  , Coordinate { x = -230, y = 32 }
  , Coordinate { x = -230, y = 33 }
  , Coordinate { x = -230, y = 34 }
  , Coordinate { x = -230, y = 35 }
  , Coordinate { x = -230, y = 36 }
  , Coordinate { x = -230, y = 37 }
  , Coordinate { x = -230, y = 38 }
  , Coordinate { x = -230, y = 39 }
  , Coordinate { x = -230, y = 40 }
  , Coordinate { x = -230, y = 41 }
  , Coordinate { x = -230, y = 42 }
  , Coordinate { x = -230, y = 43 }
  , Coordinate { x = -230, y = 44 }
  , Coordinate { x = -230, y = 45 }
  , Coordinate { x = -230, y = 46 }
  , Coordinate { x = -230, y = 47 }
  , Coordinate { x = -230, y = 48 }
  , Coordinate { x = -230, y = 49 }
  , Coordinate { x = -230, y = 50 }
  , Coordinate { x = -230, y = 51 }
  , Coordinate { x = -230, y = 52 }
  , Coordinate { x = -230, y = 53 }
  , Coordinate { x = -230, y = 54 }
  , Coordinate { x = -230, y = 55 }
  , Coordinate { x = -230, y = 56 }
  , Coordinate { x = -226, y = 3 }
  , Coordinate { x = -227, y = 3 }
  , Coordinate { x = -228, y = 3 }
  , Coordinate { x = -229, y = 3 }
  , Coordinate { x = -230, y = 3 }
  , Coordinate { x = -225, y = 6 }
  , Coordinate { x = -225, y = 5 }
  , Coordinate { x = -225, y = 4 }
  , Coordinate { x = -225, y = 3 }
  , Coordinate { x = -225, y = 7 }
  , Coordinate { x = -226, y = 7 }
  , Coordinate { x = -225, y = 8 }
  , Coordinate { x = -226, y = 8 }
  , Coordinate { x = -224, y = 9 }
  , Coordinate { x = -224, y = 8 }
  , Coordinate { x = -225, y = 10 }
  , Coordinate { x = -224, y = 10 }
  , Coordinate { x = -226, y = 10 }
  , Coordinate { x = -227, y = 9 }
  , Coordinate { x = -226, y = 9 }
  , Coordinate { x = -228, y = 11 }
  , Coordinate { x = -228, y = 10 }
  , Coordinate { x = -228, y = 9 }
  , Coordinate { x = -228, y = 12 }
  , Coordinate { x = -227, y = 12 }
  , Coordinate { x = -223, y = 11 }
  , Coordinate { x = -224, y = 11 }
  , Coordinate { x = -225, y = 11 }
  , Coordinate { x = -226, y = 11 }
  , Coordinate { x = -227, y = 11 }
  , Coordinate { x = -222, y = 11 }
  , Coordinate { x = -222, y = 12 }
  , Coordinate { x = -221, y = 9 }
  , Coordinate { x = -221, y = 10 }
  , Coordinate { x = -221, y = 11 }
  , Coordinate { x = -221, y = 12 }
  , Coordinate { x = -37, y = 8 }
  , Coordinate { x = -38, y = 8 }
  , Coordinate { x = -39, y = 8 }
  , Coordinate { x = -40, y = 8 }
  , Coordinate { x = -41, y = 8 }
  , Coordinate { x = -42, y = 8 }
  , Coordinate { x = -43, y = 8 }
  , Coordinate { x = -44, y = 8 }
  , Coordinate { x = -45, y = 8 }
  , Coordinate { x = -46, y = 8 }
  , Coordinate { x = -47, y = 8 }
  , Coordinate { x = -48, y = 8 }
  , Coordinate { x = -49, y = 8 }
  , Coordinate { x = -50, y = 8 }
  , Coordinate { x = -51, y = 8 }
  , Coordinate { x = -52, y = 8 }
  , Coordinate { x = -53, y = 8 }
  , Coordinate { x = -54, y = 8 }
  , Coordinate { x = -55, y = 8 }
  , Coordinate { x = -56, y = 8 }
  , Coordinate { x = -57, y = 8 }
  , Coordinate { x = -58, y = 8 }
  , Coordinate { x = -59, y = 8 }
  , Coordinate { x = -60, y = 8 }
  , Coordinate { x = -61, y = 8 }
  , Coordinate { x = -62, y = 8 }
  , Coordinate { x = -63, y = 8 }
  , Coordinate { x = -64, y = 8 }
  , Coordinate { x = -65, y = 8 }
  , Coordinate { x = -66, y = 8 }
  , Coordinate { x = -67, y = 8 }
  , Coordinate { x = -68, y = 8 }
  , Coordinate { x = -69, y = 8 }
  , Coordinate { x = -70, y = 8 }
  , Coordinate { x = -71, y = 8 }
  , Coordinate { x = -72, y = 8 }
  , Coordinate { x = -73, y = 8 }
  , Coordinate { x = -74, y = 8 }
  , Coordinate { x = -75, y = 8 }
  , Coordinate { x = -76, y = 8 }
  , Coordinate { x = -77, y = 8 }
  , Coordinate { x = -78, y = 8 }
  , Coordinate { x = -79, y = 8 }
  , Coordinate { x = -80, y = 8 }
  , Coordinate { x = -81, y = 8 }
  , Coordinate { x = -82, y = 8 }
  , Coordinate { x = -83, y = 8 }
  , Coordinate { x = -84, y = 8 }
  , Coordinate { x = -85, y = 8 }
  , Coordinate { x = -86, y = 8 }
  , Coordinate { x = -87, y = 8 }
  , Coordinate { x = -88, y = 8 }
  , Coordinate { x = -89, y = 8 }
  , Coordinate { x = -90, y = 8 }
  , Coordinate { x = -91, y = 8 }
  , Coordinate { x = -92, y = 8 }
  , Coordinate { x = -93, y = 8 }
  , Coordinate { x = -94, y = 8 }
  , Coordinate { x = -95, y = 8 }
  , Coordinate { x = -96, y = 8 }
  , Coordinate { x = -97, y = 8 }
  , Coordinate { x = -98, y = 8 }
  , Coordinate { x = -99, y = 8 }
  , Coordinate { x = -100, y = 8 }
  , Coordinate { x = -101, y = 8 }
  , Coordinate { x = -102, y = 8 }
  , Coordinate { x = -103, y = 8 }
  , Coordinate { x = -104, y = 8 }
  , Coordinate { x = -105, y = 8 }
  , Coordinate { x = -106, y = 8 }
  , Coordinate { x = -107, y = 8 }
  , Coordinate { x = -108, y = 8 }
  , Coordinate { x = -109, y = 8 }
  , Coordinate { x = -110, y = 8 }
  , Coordinate { x = -111, y = 8 }
  , Coordinate { x = -112, y = 8 }
  , Coordinate { x = -113, y = 8 }
  , Coordinate { x = -114, y = 8 }
  , Coordinate { x = -115, y = 8 }
  , Coordinate { x = -116, y = 8 }
  , Coordinate { x = -117, y = 8 }
  , Coordinate { x = -118, y = 8 }
  , Coordinate { x = -119, y = 8 }
  , Coordinate { x = -120, y = 8 }
  , Coordinate { x = -121, y = 8 }
  , Coordinate { x = -122, y = 8 }
  , Coordinate { x = -123, y = 8 }
  , Coordinate { x = -124, y = 8 }
  , Coordinate { x = -125, y = 8 }
  , Coordinate { x = -126, y = 8 }
  , Coordinate { x = -127, y = 8 }
  , Coordinate { x = -128, y = 8 }
  , Coordinate { x = -129, y = 8 }
  , Coordinate { x = -130, y = 8 }
  , Coordinate { x = -131, y = 8 }
  , Coordinate { x = -132, y = 8 }
  , Coordinate { x = -133, y = 8 }
  , Coordinate { x = -134, y = 8 }
  , Coordinate { x = -135, y = 8 }
  , Coordinate { x = -136, y = 8 }
  , Coordinate { x = -137, y = 8 }
  , Coordinate { x = -138, y = 8 }
  , Coordinate { x = -139, y = 8 }
  , Coordinate { x = -140, y = 8 }
  , Coordinate { x = -141, y = 8 }
  , Coordinate { x = -142, y = 8 }
  , Coordinate { x = -143, y = 8 }
  , Coordinate { x = -144, y = 8 }
  , Coordinate { x = -145, y = 8 }
  , Coordinate { x = -146, y = 8 }
  , Coordinate { x = -147, y = 8 }
  , Coordinate { x = -148, y = 8 }
  , Coordinate { x = -149, y = 8 }
  , Coordinate { x = -150, y = 8 }
  , Coordinate { x = -151, y = 8 }
  , Coordinate { x = -152, y = 8 }
  , Coordinate { x = -153, y = 8 }
  , Coordinate { x = -154, y = 8 }
  , Coordinate { x = -155, y = 8 }
  , Coordinate { x = -156, y = 8 }
  , Coordinate { x = -157, y = 8 }
  , Coordinate { x = -158, y = 8 }
  , Coordinate { x = -159, y = 8 }
  , Coordinate { x = -160, y = 8 }
  , Coordinate { x = -161, y = 8 }
  , Coordinate { x = -162, y = 8 }
  , Coordinate { x = -163, y = 8 }
  , Coordinate { x = -164, y = 8 }
  , Coordinate { x = -165, y = 8 }
  , Coordinate { x = -166, y = 8 }
  , Coordinate { x = -167, y = 8 }
  , Coordinate { x = -168, y = 8 }
  , Coordinate { x = -169, y = 8 }
  , Coordinate { x = -170, y = 8 }
  , Coordinate { x = -171, y = 8 }
  , Coordinate { x = -172, y = 8 }
  , Coordinate { x = -173, y = 8 }
  , Coordinate { x = -174, y = 8 }
  , Coordinate { x = -175, y = 8 }
  , Coordinate { x = -176, y = 8 }
  , Coordinate { x = -177, y = 8 }
  , Coordinate { x = -178, y = 8 }
  , Coordinate { x = -179, y = 8 }
  , Coordinate { x = -180, y = 8 }
  , Coordinate { x = -181, y = 8 }
  , Coordinate { x = -182, y = 8 }
  , Coordinate { x = -183, y = 8 }
  , Coordinate { x = -184, y = 8 }
  , Coordinate { x = -185, y = 8 }
  , Coordinate { x = -186, y = 8 }
  , Coordinate { x = -187, y = 8 }
  , Coordinate { x = -188, y = 8 }
  , Coordinate { x = -189, y = 8 }
  , Coordinate { x = -190, y = 8 }
  , Coordinate { x = -191, y = 8 }
  , Coordinate { x = -192, y = 8 }
  , Coordinate { x = -193, y = 8 }
  , Coordinate { x = -194, y = 8 }
  , Coordinate { x = -195, y = 8 }
  , Coordinate { x = -196, y = 8 }
  , Coordinate { x = -197, y = 8 }
  , Coordinate { x = -198, y = 8 }
  , Coordinate { x = -199, y = 8 }
  , Coordinate { x = -200, y = 8 }
  , Coordinate { x = -201, y = 8 }
  , Coordinate { x = -202, y = 8 }
  , Coordinate { x = -203, y = 8 }
  , Coordinate { x = -204, y = 8 }
  , Coordinate { x = -205, y = 8 }
  , Coordinate { x = -206, y = 8 }
  , Coordinate { x = -207, y = 8 }
  , Coordinate { x = -208, y = 8 }
  , Coordinate { x = -209, y = 8 }
  , Coordinate { x = -210, y = 8 }
  , Coordinate { x = -211, y = 8 }
  , Coordinate { x = -212, y = 8 }
  , Coordinate { x = -213, y = 8 }
  , Coordinate { x = -214, y = 8 }
  , Coordinate { x = -215, y = 8 }
  , Coordinate { x = -216, y = 8 }
  , Coordinate { x = -217, y = 8 }
  , Coordinate { x = -218, y = 8 }
  , Coordinate { x = -219, y = 8 }
  , Coordinate { x = -220, y = 8 }
  , Coordinate { x = -221, y = 8 }
  , Coordinate { x = -36, y = 8 }
  , Coordinate { x = -36, y = 7 }
  , Coordinate { x = -35, y = 7 }
  , Coordinate { x = -35, y = 8 }
  , Coordinate { x = -34, y = 7 }
  , Coordinate { x = -34, y = 8 }
  , Coordinate { x = -37, y = 6 }
  , Coordinate { x = -36, y = 6 }
  , Coordinate { x = -35, y = 6 }
  , Coordinate { x = -34, y = 6 }
  , Coordinate { x = -38, y = 3 }
  , Coordinate { x = -38, y = 4 }
  , Coordinate { x = -38, y = 5 }
  , Coordinate { x = -38, y = 6 }
  , Coordinate { x = -34, y = 2 }
  , Coordinate { x = -35, y = 2 }
  , Coordinate { x = -36, y = 2 }
  , Coordinate { x = -37, y = 2 }
  , Coordinate { x = -38, y = 2 }
  , Coordinate { x = -33, y = 1 }
  , Coordinate { x = -33, y = 2 }
  , Coordinate { x = -35, y = 0 }
  , Coordinate { x = -34, y = 0 }
  , Coordinate { x = -33, y = 0 }
  , Coordinate { x = -36, y = 0 }
  , Coordinate { x = -40, y = 1 }
  , Coordinate { x = -39, y = 1 }
  , Coordinate { x = -38, y = 1 }
  , Coordinate { x = -37, y = 1 }
  , Coordinate { x = -36, y = 1 }
  , Coordinate { x = -41, y = 1 }
  , Coordinate { x = -41, y = 2 }
  , Coordinate { x = -42, y = 1 }
  , Coordinate { x = -42, y = 2 }
  , Coordinate { x = -38, y = 0 }
  , Coordinate { x = -39, y = 0 }
  , Coordinate { x = -40, y = 0 }
  , Coordinate { x = -41, y = 0 }
  , Coordinate { x = -42, y = 0 }
  , Coordinate { x = -37, y = -1 }
  , Coordinate { x = -37, y = 0 }
  , Coordinate { x = -33, y = -2 }
  , Coordinate { x = -34, y = -2 }
  , Coordinate { x = -35, y = -2 }
  , Coordinate { x = -36, y = -2 }
  , Coordinate { x = -37, y = -2 }
  , Coordinate { x = -32, y = 0 }
  , Coordinate { x = -32, y = -1 }
  , Coordinate { x = -32, y = -2 }
  , Coordinate { x = -30, y = 1 }
  , Coordinate { x = -31, y = 1 }
  , Coordinate { x = -32, y = 1 }
  , Coordinate { x = -29, y = 1 }
  , Coordinate { x = -26, y = 2 }
  , Coordinate { x = -27, y = 2 }
  , Coordinate { x = -28, y = 2 }
  , Coordinate { x = -29, y = 2 }
  , Coordinate { x = -25, y = 2 }
  , Coordinate { x = -21, y = 3 }
  , Coordinate { x = -22, y = 3 }
  , Coordinate { x = -23, y = 3 }
  , Coordinate { x = -24, y = 3 }
  , Coordinate { x = -25, y = 3 }
  , Coordinate { x = -20, y = 2 }
  , Coordinate { x = -20, y = 3 }
  , Coordinate { x = -19, y = 1 }
  , Coordinate { x = -20, y = 1 }
  , Coordinate { x = -18, y = 3 }
  , Coordinate { x = -18, y = 2 }
  , Coordinate { x = -18, y = 1 }
  , Coordinate { x = -15, y = 4 }
  , Coordinate { x = -16, y = 4 }
  , Coordinate { x = -17, y = 4 }
  , Coordinate { x = -18, y = 4 }
  , Coordinate { x = -14, y = 0 }
  , Coordinate { x = -14, y = 1 }
  , Coordinate { x = -14, y = 2 }
  , Coordinate { x = -14, y = 3 }
  , Coordinate { x = -14, y = 4 }
  , Coordinate { x = -13, y = -1 }
  , Coordinate { x = -14, y = -1 }
  , Coordinate { x = -12, y = -5 }
  , Coordinate { x = -12, y = -4 }
  , Coordinate { x = -12, y = -3 }
  , Coordinate { x = -12, y = -2 }
  , Coordinate { x = -12, y = -1 }
  , Coordinate { x = -10, y = -6 }
  , Coordinate { x = -11, y = -6 }
  , Coordinate { x = -12, y = -6 }
  , Coordinate { x = -9, y = -6 }
  , Coordinate { x = -9, y = -7 }
  , Coordinate { x = -8, y = -6 }
  , Coordinate { x = -8, y = -7 }
  , Coordinate { x = -8, y = -5 }
  , Coordinate { x = -7, y = -6 }
  , Coordinate { x = -7, y = -5 }
  , Coordinate { x = -6, y = -7 }
  , Coordinate { x = -7, y = -7 }
  , Coordinate { x = -5, y = -4 }
  , Coordinate { x = -5, y = -5 }
  , Coordinate { x = -5, y = -6 }
  , Coordinate { x = -5, y = -7 }
  , Coordinate { x = -5, y = -3 }
  , Coordinate { x = -4, y = -1 }
  , Coordinate { x = -4, y = -2 }
  , Coordinate { x = -4, y = -3 }
  , Coordinate { x = -1, y = 0 }
  , Coordinate { x = -2, y = 0 }
  , Coordinate { x = -3, y = 0 }
  , Coordinate { x = -4, y = 0 }
  ]
