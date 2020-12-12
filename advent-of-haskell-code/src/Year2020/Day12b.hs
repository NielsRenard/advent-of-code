{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day12b
  (
  )
where

import Data.Char
import Data.Function ((&))
import Data.Tuple.Utils
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


-- Direction no longer matters for part 2
-- data Direction = N | W |  E | S deriving (Show)

type Coordinate = ( Int, Int)
type Waypoint   = ( Coordinate )
type Ferry      = ( Coordinate, Waypoint)
type Degrees    =   Int

data Instruction =
  WaypointNorth Int          | WaypointSouth Int |
  WaypointEast Int           | WaypointWest Int |
  RotateWaypointLeft Degrees | RotateWaypointRight Degrees |
  MoveTowardWaypoint Int
  deriving (Show)


type Parser = Parsec Void String

instructionParser :: Parser Instruction
instructionParser = do
  instruction <- upperChar
  value <- Lex.decimal
  case (instruction) of
    'N' -> return $ WaypointNorth value
    'S' -> return $ WaypointSouth value
    'E' -> return $ WaypointEast value
    'W' -> return $ WaypointWest value
    'R' -> return $ RotateWaypointRight value
    'L' -> return $ RotateWaypointLeft value
    'F' -> return $ MoveTowardWaypoint value

  
parseInstructions :: [String] -> [Instruction]
parseInstructions input =
  mapMaybe (parseMaybe instructionParser) input


solvePart2 :: [String] -> Int
solvePart2 input =
  let instructions       = parseInstructions input
      initFerry :: Ferry = ((0, 0), (10, 1))
      endStateFerry      = executeAllInstructions initFerry instructions
      (x,y) = (fst endStateFerry)
  in
    (abs x + abs y) -- get the manhattan distance

executeAllInstructions :: Ferry -> [Instruction] -> Ferry
executeAllInstructions ferry [] = ferry
executeAllInstructions ferry (x:xs) =
  executeAllInstructions (executeInstruction ferry x) xs

executeInstruction :: Ferry -> Instruction -> Ferry
executeInstruction ferry instruction =
  let (x, y) = (fst ferry)
      waypoint = snd ferry  
      (wpX, wpY) = waypoint
  in
    case instruction of
      WaypointNorth v ->  ((x,y), ( wpX   , (wpY+v)))
      WaypointEast  v ->  ((x,y), ((wpX+v),  wpY   ))
      WaypointSouth v ->  ((x,y), ( wpX   , (wpY-v)))
      WaypointWest  v ->  ((x,y), ((wpX-v),  wpY   ))
      RotateWaypointRight degrees ->
        ((x,y), ( rotateWaypointClockwise waypoint degrees ))
      RotateWaypointLeft degrees ->
        ((x,y), ( rotateWaypointCounterClockwise waypoint degrees ))
      MoveTowardWaypoint v -> (moveTowardWaypoint ferry v)

rotateWaypointClockwise :: Waypoint -> Degrees -> Waypoint
rotateWaypointClockwise waypoint degrees =
  let
    (x,y) = waypoint
  in
    case (degrees) of
      90  -> (  y,  (-x))
      180 -> ((-x), (-y))
      270 -> ((-y), (x)) -- had this set at ((-y), (-x)) and it took me an hour to find the bug

rotateWaypointCounterClockwise :: Waypoint -> Degrees -> Waypoint
rotateWaypointCounterClockwise waypoint degrees =
  let
    (x,y) = waypoint
  in
    case (degrees) of
      90  -> ((-y),  (x))
      180 -> ((-x), (-y))
      270 -> (( y), (-x))      
  

moveTowardWaypoint :: Ferry -> Int -> Ferry
moveTowardWaypoint ferry@((x,y),waypoint@(wpX,wpY)) steps =
  let
    stepsX = steps * wpX
    stepsY = steps * wpY
  in
    ((x+stepsX, y+stepsY), waypoint)

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/12.input"
  let ex2 = solvePart2 exinp
  let answer2 = solvePart2 input              
  putStrLn $ "Example 2: " <> show ex2        -- Example 2: 286                  
  putStrLn $ "   Part 2: " <> show answer2    --    Part 2: 62434            
                                              -- (0.02 secs, 4,010,640 bytes)

-- 26384 too low | <2020-12-12 za 23:26>
-- spent an hour debugging because, I had clockwise 270 degree rotation
-- set to ((-y), (-x)) instead of
--        ((-y),   x)

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

myExinp :: [String]
myExinp =
    [
      "F1"
    ]    

exinpShould00 :: [String]
exinpShould00 =
  [
    "F10",
    "R180",
    "F20",
    "L180",
    "F20",
    "R90",
    "R90",
    "F20",
    "L90",
    "L90",
    "F10"
  ]
