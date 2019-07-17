{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Year2016.Day01
  (draw)
where

import           Data.Char
import           Data.Maybe
import           Prelude                        ( last
                                                , print
                                                , read
                                                )
import           RIO                     hiding ( many )
import           RIO.List                      as L
import           RIO.List.Partial              as L'
import           RIO.Text                      as T
import           Text.ParserCombinators.ReadP
import           Diagrams.Prelude               (crimson, square, Diagram, circle, square, regPoly, fc, (#), blue, atop, showOrigin, atPoints, trailVertices)
import           Diagrams.Backend.SVG.CmdLine

{--
part 1
Wanted to learn how to use Parser Combinators so did that. Still hardcoding the max three digits for the numbers parser. Think I can change that by using many1.

part 2
Read the assignment wrong initially, thinking it was looking for the first coordinate that it *lands* (and turns) on twice.
--}

myCircle :: Diagram B
myCircle = circle 0.2 # fc blue # showOrigin

mySquare :: Diagram B
mySquare = square 1 # fc crimson

myBeads :: Diagram B
myBeads = atPoints (trailVertices $ regPoly 6 1) (repeat myCircle)

draw = mainWith mySquare

answers :: IO (Int, Int)
answers = do
  input <- readFileUtf8 "data/2016/input/input_2016_01_a.txt"
  let
--    input = T.pack "R1, R1, R1, R1, R1, R1, R1" -- uncomment to test with hardcoded string
      moves         = parseString input
      answerPartOne = solvePartOne initialPosition moves
      answerPartTwo = solvePartTwo initialPosition moves
  pure (answerPartOne, answerPartTwo)
 where
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
