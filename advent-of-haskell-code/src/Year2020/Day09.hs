{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day09 where

import Control.Lens
import Data.Char
import Data.Function ((&))
import Data.List as L
import Data.List.Index
import qualified Data.List.Split as Split
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import RIO hiding (many, trace, (.~))
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, separatorChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Utils

type Parser = Parsec Void String

solvePart1 :: Int -> [Int] -> Int
solvePart1 preamble input =
  checkIfConforms preamble input

checkIfConforms :: Int -> [Int] -> Int
checkIfConforms preamble [] = 0
checkIfConforms preamble xs =
  let
    previous = take preamble xs
    number = head $ drop preamble xs
  in
    if isSumOfTwoPrevious previous number
    then 0 + checkIfConforms preamble (drop 1 xs)
    else number
  

isSumOfTwoPrevious :: [Int] -> Int -> Bool
isSumOfTwoPrevious previous number =
  not $ null [ True | x <- previous , y <- previous , z <- [number], x + y == z , x /= y]
  

main :: IO ()
main = do
  input <- map (\s -> read s :: Int) <$> lines <$> readFile "data/2020/9.input"
  let ex1 = solvePart1 5 exinp
  let answer1 = solvePart1 25 input
  --  let ex2 = solvePart2 exinp2
  --  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
  --  putStrLn $ "Example 2: " <> show ex2
  --  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [Int]
exinp =  [35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576]
