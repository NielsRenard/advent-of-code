module Year2019.Day08 where

-- attempting with no (direct) string operations
import Data.Function ((&))
import Data.List
import Data.List.Index
import Data.List.Split (chunksOf)
import Utils
import Year2019.Input.Day08 (input)

solvePartOne =
  let layers = layers25x6 input
      fewestZerosLayer = layers !! fst (head $ sortOn snd $ indexed $ map countZerosInLayer layers)
      ones = length . concatMap (filter (== 1)) $ fewestZerosLayer
      twos = length . concatMap (filter (== 2)) $ fewestZerosLayer
   in ones * twos

solvePartTwo =
  renderLayer $ chunksOf 25 $ allLayersPixels (layers25x6 input) 25 6

allLayersPixels layers width height =
  [ visiblePixel $ map ((!! that) . map (!! it)) layers
    | that <- [0 .. pred height],
      it <- [0 .. pred width]
  ]

visiblePixel :: [Integer] -> Integer
visiblePixel stackedPixel = head $ dropWhile (== 2) stackedPixel

layers25x6 = chunksOf 6 . chunksOf 25

-- silly that these aren't just one function ---------------------
countZerosInLayer :: (Eq a, Num a) => [[a]] -> Int
countZerosInLayer = length . concatMap (filter (== 0))

-- render functions ----------------------------------------------------
renderLayer :: [[Integer]] -> IO ()
renderLayer layer = mapM_ putStrLn $ (map $ map renderPixel) layer

renderPixel :: Integer -> Char
renderPixel x = case x of
  0 -> ' '
  1 -> '#'
  2 -> '?'

------------------------------------------------------------------------
