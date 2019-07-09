{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day01
  ()
where

import           Data.Char
import           Data.Maybe
import           Prelude                        ( last
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
--    example = T.pack "R2, L10"                       -- should give 12
--    ws            = splitCommaAndStrip example       -- uncomment to test with hardcoded input
    ws  = splitCommaAndStrip contents
    all = L.map (fst . last . readP_to_S move . T.unpack) ws
    initPos =
      Position { coordinate = Coordinate { x = 0, y = 0 }, facing = North }
    endPos   = L.foldl translate initPos all
    distance = taxicabDiff initPos endPos  -- {x = -173, y = -159} facing South
  pure distance


taxicabDiff :: Position -> Position -> Int
taxicabDiff p1 p2 =
  let x1 = xco p1
      x2 = xco p2
      y1 = yco p1
      y2 = yco p2
  in  abs (x1 - x2) + abs (y1 - y2)
 where
  xco p = x $ coordinate p
  yco p = y $ coordinate p


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
  n <- count 3 digit <|> count 2 digit <|> count 1 digit
  return (read n)

-- cleaning function
splitCommaAndStrip :: Text -> [Text]
splitCommaAndStrip = L.map T.strip . T.split (== ',')
