{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Year2020.Day25
  (
  )
where

import Data.Char
import Data.Ord (comparing)
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
import qualified Data.HashMap.Strict as HM
import Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

{- part 1 -}

solvePart1 :: (Int, Int) -> Int
solvePart1 input@(cardPubKey,doorPubKey) =
  let cardLoopSize = findLoopSize cardPubKey
  in
    breakEncryption cardLoopSize doorPubKey

findLoopSize :: Int -> Int
findLoopSize publicKey =
  snd $ until ((== publicKey) . fst)   (\(value, i) -> ((value * 7 `mod` 20201227), (succ i))) (1,0)

breakEncryption loopSize subjectNumber =
  fst $ until ((== loopSize) . snd) (\(value, i) -> ((value * subjectNumber `mod` 20201227), (succ i))) (1, 0)

main = do
  input <- map read <$> lines <$> readFile "data/2020/25.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 (head input, last input)
--  let ex2 = solvePart2 exinp
--  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: (Int, Int)
exinp = (5764801, 17807724)

inp :: (Int, Int)
inp = (17773298,15530095)
