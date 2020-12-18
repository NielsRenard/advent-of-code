{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}


module Year2020.Day17
  (
  )
where

import Data.List as L
import Data.Tuple
import Data.Tuple.Utils
import Data.Bifunctor
import Data.Maybe
import System.Directory
import Data.Ord (comparing)

type X = Int
type Y = Int
type Z = Int
type Coord3D = (X, Y, Z)
data State = Active | Inactive deriving (Show, Eq)
type Matrix3D = [(Coord3D, State)]


parsePixel :: Char -> State
parsePixel =
  \case
    '#' -> Active
    '.' -> Inactive

--         z-1                z0                   z1 
--    [nwB] [nB] [neB]      [nw] [n] [ne]       [nwF] [nF] [neF] 
--    [wB]  [xB]  [eB]      [w]  [x]  [e]       [wF]  [xF]  [eF]
--    [swB] [sB] [seB]      [sw] [s] [se]       [swF] [sF] [seF]
neighbors :: Coord3D -> Matrix3D -> [Coord3D]
neighbors coord@(x,y,z) matrix =
  let 
    nwB = (( x - 1 ), ( y - 1 ), (z - 1))
    nB  = (( x     ), ( y - 1 ), (z - 1))
    neB = (( x + 1 ), ( y - 1 ), (z - 1))
    wB  = (( x - 1 ), ( y     ), (z - 1))
    xB  = (( x     ), ( y     ), (z - 1))
    eB  = (( x + 1 ), ( y     ), (z - 1))
    swB = (( x - 1 ), ( y + 1 ), (z - 1))
    sB  = (( x     ), ( y + 1 ), (z - 1))
    seB = (( x + 1 ), ( y + 1 ), (z - 1))
    nw  = (( x - 1 ), ( y - 1 ), ( z   ))
    n   = (( x     ), ( y - 1 ), ( z   ))
    ne  = (( x + 1 ), ( y - 1 ), ( z   ))
    w   = (( x - 1 ), ( y     ), ( z   ))
    e   = (( x + 1 ), ( y     ), ( z   ))
    sw  = (( x - 1 ), ( y + 1 ), ( z   ))
    s   = (( x     ), ( y + 1 ), ( z   ))
    se  = (( x + 1 ), ( y + 1 ), ( z   ))
    nwF = (( x - 1 ), ( y - 1 ), (z + 1))
    nF  = (( x     ), ( y - 1 ), (z + 1))
    neF = (( x + 1 ), ( y - 1 ), (z + 1))
    wF  = (( x - 1 ), ( y     ), (z + 1))
    xF  = (( x     ), ( y     ), (z + 1))
    eF  = (( x + 1 ), ( y     ), (z + 1))
    swF = (( x - 1 ), ( y + 1 ), (z + 1))
    sF  = (( x     ), ( y + 1 ), (z + 1))
    seF = (( x + 1 ), ( y + 1 ), (z + 1))
  in
    concat [[nwB, nB , neB, wB, xB, eB , swB, sB,  seB]
          ,[nw,  n  , ne,  w ,      e , sw,  s,   se]
          ,[nwF, nF , neF, wF, xF, eF , swF, sF,  seF]]
    
--         z-1                z0                   z1 
--    [nwB] [nB] [neB]      [nw] [n] [ne]       [nwF] [nF] [neF] 
--    [wB]  [xB]  [eB]      [w]  [x]  [e]       [wF]  [xF]  [eF]
--    [swB] [sB] [seB]      [sw] [s] [se]       [swF] [sF] [seF]
getActiveNeighbors :: Coord3D -> Matrix3D -> [State]
getActiveNeighbors coord@(x,y,z) matrix =
  let 
    nwB = lookup (( x - 1 ), ( y - 1 ), (z - 1)) matrix
    nB  = lookup (( x     ), ( y - 1 ), (z - 1)) matrix
    neB = lookup (( x + 1 ), ( y - 1 ), (z - 1)) matrix
    wB  = lookup (( x - 1 ), ( y     ), (z - 1)) matrix
    xB  = lookup (( x     ), ( y     ), (z - 1)) matrix    
    eB  = lookup (( x + 1 ), ( y     ), (z - 1)) matrix
    swB = lookup (( x - 1 ), ( y + 1 ), (z - 1)) matrix
    sB  = lookup (( x     ), ( y + 1 ), (z - 1)) matrix
    seB = lookup (( x + 1 ), ( y + 1 ), (z - 1)) matrix
    nw  = lookup (( x - 1 ), ( y - 1 ), ( z   )) matrix
    n   = lookup (( x     ), ( y - 1 ), ( z   )) matrix
    ne  = lookup (( x + 1 ), ( y - 1 ), ( z   )) matrix
    w   = lookup (( x - 1 ), ( y     ), ( z   )) matrix
    e   = lookup (( x + 1 ), ( y     ), ( z   )) matrix
    sw  = lookup (( x - 1 ), ( y + 1 ), ( z   )) matrix
    s   = lookup (( x     ), ( y + 1 ), ( z   )) matrix
    se  = lookup (( x + 1 ), ( y + 1 ), ( z   )) matrix
    nwF = lookup (( x - 1 ), ( y - 1 ), (z + 1)) matrix
    nF  = lookup (( x     ), ( y - 1 ), (z + 1)) matrix
    neF = lookup (( x + 1 ), ( y - 1 ), (z + 1)) matrix
    wF  = lookup (( x - 1 ), ( y     ), (z + 1)) matrix
    xF  = lookup (( x     ), ( y     ), (z + 1)) matrix    
    eF  = lookup (( x + 1 ), ( y     ), (z + 1)) matrix
    swF = lookup (( x - 1 ), ( y + 1 ), (z + 1)) matrix
    sF  = lookup (( x     ), ( y + 1 ), (z + 1)) matrix
    seF = lookup (( x + 1 ), ( y + 1 ), (z + 1)) matrix
  in
    filter (== Active) $ catMaybes $ concat [[nwB, nB , neB, wB, xB, eB , swB, sB,  seB]
                                        ,[nw,  n  , ne,  w ,      e , sw,  s,   se]
                                        ,[nwF, nF , neF, wF, xF, eF , swF, sF,  seF]]


parseInputToMatrix :: String -> Matrix3D
parseInputToMatrix input =
  let split = lines input
      height = length split
      width  = length $ head split
  in
    [ ((x, y, 0), parsePixel (split !! y !! x)) | x<-[0..(pred width)], y<-[0..(pred height)]]

{- part 1
  Simulate six cycles of three dimensional conway game.
  Then check how many active.

  Active   with 2 or           3 neighbors active  -> remain Active
  Active   with 1 or more than 3 neighbors active  -> become Inactive
-}

solvePart1 :: String -> Matrix3D
solvePart1 input =
  let matrix            :: Matrix3D           = parseInputToMatrix input
      neighborsOfActive :: [Coord3D]          = getNeighborsOfActive matrix
  in
    do
      matrixAfterOneStep    <- pure $ loop neighborsOfActive matrix
      matrixAfterTwoSteps   <- pure $ loop (getNeighborsOfActive matrixAfterOneStep) matrixAfterOneStep
      matrixAfterThreeSteps <- pure $ loop (getNeighborsOfActive matrixAfterTwoSteps) matrixAfterTwoSteps
      matrixAfterFourSteps  <- pure $ loop (getNeighborsOfActive matrixAfterThreeSteps) matrixAfterThreeSteps
      matrixAfterFiveSteps  <- pure $ loop (getNeighborsOfActive matrixAfterFourSteps) matrixAfterFourSteps            
      matrixAfterSixSteps   <- pure $ loop (getNeighborsOfActive matrixAfterFiveSteps) matrixAfterFiveSteps            
      matrixAfterSixSteps

getNeighborsOfActive :: Matrix3D -> [Coord3D]
getNeighborsOfActive m = nub $ concatMap (\it -> neighbors (fst it) m) $ filter ((== Active) . snd) m

loop :: [Coord3D] -> Matrix3D -> Matrix3D
loop coords matrix =
  filter ((== Active) . snd) $ map (\coord -> advancePixel coord matrix) coords

advancePixel :: Coord3D -> Matrix3D -> (Coord3D, State)
advancePixel coord matrix =
  let
    state           = case lookup coord matrix of (Just state') -> state'; Nothing -> Inactive
    activeNeighbors = length $ getActiveNeighbors coord matrix
  in
  case (state, activeNeighbors) of
    (Active,   2) -> (coord, Active)
    (Active,   3) -> (coord, Active)
    (Inactive, 3) -> (coord, Active)
    otherwise     -> (coord, Inactive)


main :: IO ()
main = do
  input <- readFile "data/2020/17.input"
  let ex1 = length $ solvePart1 exinp
  let answer1 = length $ solvePart1 input
--  let ex2 = solvePart2 exinp2
--  let answer2 = solvePart2 input           -- *Year2020.Day17> main         
  putStrLn $ "Example 1: " <> show ex1       -- Example 1: 112                
  putStrLn $ "   Part 1: " <> show answer1   --    Part 1: 324                
--  putStrLn $ "Example 2: " <> show ex2     -- (1.84 secs, 124,841,032 bytes)
--  putStrLn $ "   Part 2: " <> show answer2


{-- Test and example input --}

exinp :: String
exinp = ".#.\n\
        \..#\n\
        \###"

input :: String
input = "#.#..#.#\n\
        \#.......\n\
        \####..#.\n\
        \.#.#.##.\n\
        \..#..#..\n\
        \###..##.\n\
        \.#..##.#\n\
        \.....#.."

{- printing functions -}

slicePrint :: String -> IO ()
slicePrint = putStrLn

showPixel :: State -> Char
showPixel =
  \case
    Inactive -> '.'
    Active   -> '#'


{- debug:

after one step:
sortBy (comparing (thd3 .fst)) $ nub $ filter ((== Active) . snd) $ solvePart1 exinp
[((0,1,-1),Active),((1,3,-1),Active),((2,2,-1),Active),
  ((0,1,0),Active), ((1,2,0),Active), ((1,3,0),Active), ((2,1,0),Active),((2,2,0),Active),
  ((0,1,1),Active), ((1,3,1),Active), ((2,2,1),Active)]

 ...
 #..
 ..#
 .#.

 ...
 # #
 .##
 .#.

 ...
 #..
 ..#
 .#.

-}
