{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day12
  (
  )
where

import Data.Char
import Data.Function ((&))
import Data.List as L
import Data.List.Index
import Data.Maybe
import Data.String
import Debug.Trace (trace)
import RIO hiding (many, trace, (.~), try)
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar, alphaNumChar, char, separatorChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Utils


data Direction = N | W |  E | S deriving (Show)

type Coordinate = (Int, Int)
type Ferry = (Coordinate, Direction)
type Degrees = Int

data Instruction =
  North Int        | South Int | East Int | West Int |
  TurnLeft Degrees | TurnRight Degrees |
  MoveForward Int
  deriving (Show)


type Parser = Parsec Void String

instructionParser :: Parser Instruction
instructionParser = do
  instruction <- upperChar
  value <- Lex.decimal
  case (instruction) of
    'N' ->
      return $ North value
    'S' ->
      return $ South value
    'E' ->
      return $ East value
    'W' ->
      return $ West value
    'L' ->
      return $ TurnLeft value
    'R' ->
      return $ TurnRight value
    'F' ->
      return $ MoveForward value

  
parseInstructions :: [String] -> [Instruction]
parseInstructions input =
  mapMaybe (parseMaybe instructionParser) input


solvePart1 :: [String] -> Int
solvePart1 input =
  let instructions       = parseInstructions input
      initFerry :: Ferry = ((0, 0), E)
      endStateFerry      = executeAllInstructions initFerry instructions
      (x,y) = (fst endStateFerry)
  in
    (abs x + abs y)

executeAllInstructions :: Ferry -> [Instruction] -> Ferry
executeAllInstructions ferry [] = ferry
executeAllInstructions ferry (x:xs) =
  executeAllInstructions (executeInstruction ferry x) xs

executeInstruction :: Ferry -> Instruction -> Ferry
executeInstruction ferry instruction =
  let (x, y) = (fst ferry)
      direction = snd ferry
  in
    case instruction of
      North v ->  ((x,(y+v)), direction)
      East  v ->  ((x+v,y),   direction)
      South v ->  ((x,(y-v)), direction)
      West  v ->  ((x-v,y),   direction)
      TurnLeft degrees ->  ((x,y), ( turnLeft direction degrees  ))
      TurnRight degrees -> ((x,y), ( turnRight direction degrees ))
      MoveForward v -> (moveForward ferry v)

moveForward :: Ferry -> Int -> Ferry
moveForward ferry steps =
  let
    (x,y) = (fst ferry)
    direction = (snd ferry)
  in
  case direction of
    N -> ((x, y+steps), direction)
    E -> ((x+steps, y), direction)
    S -> ((x, y-steps), direction)
    W -> ((x-steps, y), direction)

turnLeft :: Direction -> Degrees -> Direction
turnLeft currDirection degrees =
  case (currDirection, degrees) of
    ( N , 90  ) -> W
    ( N , 180 ) -> S
    ( N , 270 ) -> E
    ( E , 90  ) -> N
    ( E , 180 ) -> W
    ( E , 270 ) -> S
    ( S , 90  ) -> E
    ( S , 180 ) -> N
    ( S , 270 ) -> W
    ( W , 90  ) -> S
    ( W , 180 ) -> E
    ( W , 270 ) -> N

turnRight :: Direction -> Degrees -> Direction
turnRight currDirection degrees =
  case (currDirection, degrees) of
    ( N , 90  ) -> E
    ( N , 180 ) -> S
    ( N , 270 ) -> W
    ( E , 90  ) -> S
    ( E , 180 ) -> W
    ( E , 270 ) -> N
    ( S , 90  ) -> W
    ( S , 180 ) -> N
    ( S , 270 ) -> E
    ( W , 90  ) -> N
    ( W , 180 ) -> E
    ( W , 270 ) -> S    

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/12.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  putStrLn $ "Example 1: " <> show ex1     
  putStrLn $ "   Part 1: " <> show answer1 

-- Part two is in a separate file Day12b.hs
  
{-- Test and example input --}

exinp :: [String]
exinp =
    [
      "F10",
      "N3",
      "F7",
      "R90",
      "F11"
    ]
