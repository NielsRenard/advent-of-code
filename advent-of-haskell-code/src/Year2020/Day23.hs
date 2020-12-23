{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Year2020.Day23
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

{- part 1 -}

parseInput input =
  map (read :: String -> Int) $ filter (/= "\n") $ chunksOf 1 input

--solvePart1 :: String -> Int
solvePart1 input =
  let cups = parseInput input
      amountOfCups = length cups
  in
    map intToDigit $ take (amountOfCups - 1) $ drop 1 $ dropWhile (/= 1) $ cycle $ loop cups 0 1

loop :: [Int] -> Int -> Int -> [Int]
loop cups cursor move =
  let numberOfCups            = length cups
      newCursor               = if (succ cursor) == numberOfCups then 0 else (succ cursor)
      circleCups              = cycle cups
      current                 = circleCups !! cursor
      pickUp                  = take 3 $ drop (succ cursor) circleCups
      withoutPickup           = filter (\it -> not (it `elem` pickUp)) cups 
      withoutPickupSequential = take (succ cursor) circleCups ++ (take (numberOfCups - (cursor + 4)) $ drop (cursor + 4) circleCups)
      destination             = findFirstLower withoutPickup current
      destIdx                 = fromJust $ elemIndex destination withoutPickup
      newCups                 = take (destIdx + 1) withoutPickup ++ pickUp ++ drop (destIdx + 1) withoutPickup
      cursorOffset            = if destIdx < cursor then fromJust $ (\it -> it - cursor) <$> (elemIndex current newCups)  else 0
      newCupsRetainCursor     = take numberOfCups $ drop cursorOffset $ cycle newCups
  in
--    (trace $ "--move : " <> show move <> " --") $
--    (trace $ "cups:              " <> show cups) $
--    (trace $ "current:           " <> show current) $
--    (trace $ "pick up:           " <> show pickUp) $     
--    (trace $ "without pickup:    " <> show withoutPickup) $     
--    (trace $ "destination:       " <> (show $ destination )) $
--    (trace $ "new cups:          " <> (show $ newCups )) $
--    (trace $ "cursor:            " <> show cursor) $
--    (trace $ "cursor offset:     " <> (show $ cursorOffset )) $        
--    (trace $ "new cups: " <> (show $ newCupsRetainCursor )) $    
--    (trace "") $
    if move == 100
    then newCupsRetainCursor
    else loop newCupsRetainCursor newCursor (succ move)
  where
    findFirstLower xs n =
      case find (== (pred n)) xs of
        Just v  -> v
        Nothing -> if (n == minimum xs)
                   then maximum xs
                   else findFirstLower xs (pred n)
      
main = do
  input <- readFile "data/2020/23.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
--  let ex2 = solvePart2 exinp
--  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2

-- 278659341 too high

{-- Test and example input --}

exinp :: String
exinp = T.unpack $
  [text|389125467|]
    
inp :: String
inp =  T.unpack $
  [text|872495136|]
