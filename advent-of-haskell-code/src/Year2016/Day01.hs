{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day01
  (
  ) where

import           Data.Char
import           Prelude                      (head, print, read)
import           RIO
import           RIO.List                     as L
import           RIO.Text                     as T
import           Text.ParserCombinators.ReadP

main :: IO Text
main = do
  contents <- readFileUtf8 "data/2016/input/input_2016_01_a.txt"
  let ws = T.split (== ',') contents
      fst =
        case (headMaybe ws) of
          Just x  -> x
          Nothing -> error "no"
  print fst
  pure fst

data Step = Step
  { direction :: Compass
  , amount    :: Int
  } deriving (Show)

data Compass
  = North
  | East
  | South
  | West
  deriving (Show)

step :: ReadP Step
step = do
  direction <- compass
  steps <- numbers
  return (Step direction steps)

compass :: ReadP Compass
compass = do
  c' <- char 'N' <|> char 'W' <|> char 'S' <|> char 'E'
  return
    (case c' of
       'N' -> North
       'E' -> East
       'S' -> South
       'W' -> West)

digit :: ReadP Char
digit = satisfy isDigit

numbers :: ReadP Int
numbers = do
  n <- count 1 digit <|> count 2 digit <|> count 3 digit
  return (read n)
