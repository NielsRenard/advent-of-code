{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day01 () where


import           Data.Char
import           Prelude                      (head, print, read)
import           RIO                          hiding ((<|>))
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

compassDirection :: ReadP Char
compassDirection = satisfy (\char ->
              L.any (char ==) "NWSE")

number :: ReadP Char
number = satisfy isDigit
