{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day01
  (
  ) where

import           Data.Char
import           Data.Maybe
import           Prelude                      (head, print, read)
import           RIO hiding (many)
import           RIO.List                     as L
import           RIO.Text                     as T
import           Text.ParserCombinators.ReadP

main :: IO [(Move, String)]
main = do
  contents <- readFileUtf8 "data/2016/input/input_2016_01_a.txt"
  let ws = L.map T.strip $ T.split (== ',') contents
      all = L.map (head . readP_to_S move . T.unpack)  ws
  pure all

data Move = Move
  { direction :: Turn
  , steps     :: Int
  } deriving (Show)

data Turn
  = TurnLeft
  | TurnRight
  deriving (Show)

move :: ReadP Move
move = do
  direction <- turn
  steps <- numbers
  return (Move direction steps)

turn :: ReadP Turn
turn = do
  t' <- char 'L' <|> char 'R'
  return
    (case t' of
       'L' -> TurnLeft
       'R' -> TurnRight)

digit :: ReadP Char
digit = satisfy isDigit

numbers :: ReadP Int
numbers = do
  n <- count 1 digit <|> count 2 digit <|> count 3 digit
  return (read n)
