{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day01 () where


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
        fst = case (headMaybe ws) of
          Just x  -> x
          Nothing -> error "no"
    print fst
    pure fst

data Step = Step { direction :: Char
                 , amount    :: Int } deriving Show

step :: ReadP Step
step = do
  direction <- char 'N' <|> char 'W' <|> char 'S' <|> char 'E'
  steps <- count 1 digit <|> count 2 digit <|> count 3 digit
  return Step {direction = direction, amount = read steps}

digit :: ReadP Char
digit = satisfy isDigit

step :: ReadP (Char, Int)
step = do
  direction <- char 'N' <|> char 'W' <|> char 'S' <|> char 'E'
  steps <- count 2 digit
  return (direction, read steps)
