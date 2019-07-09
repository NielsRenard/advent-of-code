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

main :: IO [(Move, String)]
main = do
  contents <- readFileUtf8 "data/2016/input/input_2016_01_a.txt"
  let ws    = splitCommaAndStrip contents
      all   = L.map (head . readP_to_S move . T.unpack) ws
      three = L.take 5 all
  pure three

--  print ws
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

data Coordinate = Coordinate
  { x :: Int
  , y :: Int
  } deriving (Show)

translate :: Coordinate -> Compass -> Move -> Coordinate
translate coord compass move =
  let x' = x coord
      y' = y coord
      n  = steps move
  in  case direction move of
        TurnLeft -> case compass of
          North -> Coordinate (x' - n) y'
          East  -> Coordinate x' (y' + n)
          South -> Coordinate (x' - n) y'
          West  -> Coordinate x' (y' - n)
        TurnRight -> case compass of
          North -> Coordinate (x' + n) y'
          East  -> Coordinate x' (y' + n)
          South -> Coordinate (x' + n) y'
          West  -> Coordinate x' (y' + n)

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
