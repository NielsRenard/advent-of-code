
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Year2020.Day22
  (
  )
where

import Data.Char
import Data.List as L
import Data.List.Utils
import Data.List.Split as Split hiding (sepBy)
import qualified Data.Set as S
import Data.List.Index
import Data.Maybe
import Data.Text as T (unpack)
import NeatInterpolation (text)
import Debug.Trace (trace)
import System.Directory
import Data.Void
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type Ingredient = String
type Allergen = String
type Food = ([Ingredient], [Allergen])

type Player = Int
type Card = Int
type Deck = (Player, [Card])

{- part 1 -}

deckParser :: Parser Deck
deckParser = do
  playerString <- string "Player"
  space
  playerNumber <- decimal
  colon <- char ':'
  eol
  numbers <- decimal `sepEndBy` newline
  return $ (playerNumber, numbers)
  
parseInput input =
  let splitInput = Split.splitOn "\n\n" input
      twoDecks = mapMaybe (parseMaybe deckParser) splitInput
  in
    twoDecks

solvePart1 :: String -> Int
solvePart1 input =
  let
    (player1, player2) = (head $ parseInput input, last $ parseInput input)
    amountOfValues =  sum [length (snd player1), length (snd player2)]
  in
    sum $ zipWith (*) (snd (loop (player1, player2)))  (reverse [1..amountOfValues])

loop :: (Deck, Deck) -> Deck
loop (player1Deck, (_, []))     = player1Deck
loop ((_, [])    , player2Deck) = player2Deck
loop ((p1, (x:xs)), (p2, (y:ys))) =
  if x > y
  then loop ((p1, (xs ++ [x,y])), (p2, ys))
  else loop ((p1, xs)           , (p2, (ys ++ [y, x])))
                            

main = do
  input <- readFile "data/2020/22.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
--  let ex2 = solvePart2 exinp
--  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: String
exinp = T.unpack $
  [text|
       Player 1:
       9
       2
       6
       3
       1
       
       Player 2:
       5
       8
       4
       7
       10
       |]
    
