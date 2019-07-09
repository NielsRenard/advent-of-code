{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day01
  ()
where

import           Data.Char
import           Data.Maybe
import           Prelude                        ( head
                                                , print
                                                , read
                                                )
import           RIO                     hiding ( many )
import           RIO.List                      as L
import           RIO.Text                      as T
import           Text.ParserCombinators.ReadP

main :: IO Int
main = do
  contents <- readFileUtf8 "data/2016/input/input_2016_01_a.txt"
  let
    exampleString = T.pack "R2, R2, R2"
--    ws    = splitCommaAndStrip exampleString   -- flip next to line comments
    ws    = splitCommaAndStrip contents      -- to test with hardcoded string
    all   = L.map (fst . head . readP_to_S move . T.unpack) ws
    first = head all
    initPos =
      Position { coordinate = Coordinate { x = 0, y = 0 }, facing = North }
    endPos = L.foldl walk initPos all
    distance = taxicabDiff initPos endPos  -- {x = 18, y = -17} facing South
  pure distance


taxicabDiff :: Position -> Position -> Int
taxicabDiff p1 p2  = let x1 = x $ coordinate p1
                         x2 = x $ coordinate p2
                         y1 = y $ coordinate p1
                         y2 = y $ coordinate p2
                     in
                       abs (x1 - x2) + abs (y1 - y2)

walk :: Position -> Move -> Position
walk position m = translate position m

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
                         } deriving (Show)

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving (Show)

translate :: Position -> Move -> Position
translate position move =
  let x'      = x $ coordinate position
      y'      = y $ coordinate position
      compass = facing position
      n       = steps move
  in  case direction move of
        TurnLeft -> case compass of
          North -> Position (Coordinate (x' - n) y') West
          East  -> Position (Coordinate x' (y' + n)) North
          South -> Position (Coordinate (x' + n) y') East
          West  -> Position (Coordinate x' (y' - n)) South
        TurnRight -> case compass of
          North -> Position (Coordinate (x' + n) y') East
          East  -> Position (Coordinate x' (y' - n)) South
          South -> Position (Coordinate (x' - n) y') West
          West  -> Position (Coordinate x' (y' + n)) North

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
  n <- count 1 digit <|> count 2 digit <|> count 3 digit
  return (read n)

splitCommaAndStrip :: Text -> [Text]
splitCommaAndStrip = L.map T.strip . T.split (== ',')
