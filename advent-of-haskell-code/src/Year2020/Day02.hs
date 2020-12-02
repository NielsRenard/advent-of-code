{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 

module Year2020.Day02 where

import Data.Function ((&))
import Data.List as L
import Data.List.Index
import qualified Data.Text as T
import Data.String
import Data.List.Split (chunksOf)
import Data.Text as T (Text, pack, unpack)
import Utils
import Debug.Trace (trace)
import System.Directory
import Data.Maybe

validPassword ::  Int -> String -> Bool
validPassword partOneOrTwo passwordString =
  let
    rules =  head . words $ passwordString
    password = last . words $ passwordString 
    minChars :: Int = read (T.unpack $ head $ T.splitOn "-" $ T.pack rules) :: Int
    maxChars :: Int = read (T.unpack $ last $ T.splitOn "-" $ T.pack rules) :: Int
    reqChar = head $ words passwordString !! 1
    groupedChars = group password
    countedChars = L.length (L.filter (== reqChar) password)
  in
    if partOneOrTwo == 1
    then
      -- rules for part 1
      (countedChars >= minChars && countedChars <= maxChars)
    else
      -- rules for part 2
      ((password !! (pred minChars)) == reqChar) `xor` ((password !! (pred maxChars)) == reqChar)

xor x y
  | x == True && y == False = True
  | x == False && y == True = True
  | otherwise = False
  

part1 :: [String] -> Int
part1 input = L.length $ L.filter (== True) $ L.map (validPassword 1) input

part2 :: [String] -> Int
part2 input = L.length $ L.filter (== True) $ L.map (validPassword 2) input

exampleInput =
  [ "1-3 a: abcde", 
    "1-3 b: cdefg", 
    "2-9 c: ccccccccc"
  ]

main :: IO ()
main = do
  listOfStrings <- lines <$> readFile "data/2020/2.input"  
  let answer1 = part1 listOfStrings
  let answer2 = part2 listOfStrings  
  putStrLn $ show answer1
  putStrLn $ show answer2
