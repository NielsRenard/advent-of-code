{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day01 () where


import           Prelude                      (print)
import           RIO
import           RIO.List as L
import           RIO.Text                     as T
import           Text.Parsec ((<|>))
import           Text.ParserCombinators.ReadP 
import Text.Parsec.String (Parser)
import Data.Char



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
                 , stepCount :: Int } deriving Show


--stepParser :: readP Int
--stepParser  = do
--  direction <- char 'N'
--  stepCount <- numbers 1
--  return stepCount

isVowel :: Char -> Bool
isVowel char =
    L.any (char ==) "aouei"

vowel :: ReadP Char
vowel =
    satisfy isVowel
