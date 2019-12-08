module Year2019.Day08 where
-- attempting with no (direct) string operations
import Data.Function ((&))
import Data.List
import Year2019.Input.Day08 (input)
import Data.List.Index
import Data.List.Split (chunksOf)
import Utils

exampleInput :: [Integer]
exampleInput = [1,2,3,4,5,6,7,8,9,0,1,2]

myExampleInput :: [Integer]
myExampleInput = [1,2,3,0,0,0,7,8,9,0,1,2]

--layerize = chunksOf 25

-- 2704 no too high
-- 2288 no

solvePartOne=
  let layers = layers25x6 input
      fewestZerosLayerIndex = fst $ head $ sortOn snd $ indexed $ map countZerosInLayer layers
      fewestZerosLayer = layers !! fewestZerosLayerIndex
      ones = countOnesInLayer $ fewestZerosLayer
      twos = countTwosInLayer $ fewestZerosLayer      
  in
    ones * twos


layers25x6 = chunksOf 6 . chunksOf 25
layers3x2 = chunksOf 2 . chunksOf 3


countZerosInLayer :: (Eq a , Num a) => [[a]] -> Int
countZerosInLayer = length . concatMap (filter (== 0))

countOnesInLayer :: (Eq a , Num a) => [[a]] -> Int
countOnesInLayer = length . concatMap (filter (== 1))

countTwosInLayer :: (Eq a , Num a) => [[a]] -> Int
countTwosInLayer = length . concatMap (filter (== 2))
  
-- don't need this
allZeros :: (Eq a, Num a) => Int -> [[a]] -> Int
allZeros width = length . filter (== replicate width 0)




