module Year2019.Day04 where

import Data.Char
import Data.Function ((&))
import Data.List (group)
import Data.List.HT hiding (group)
import Data.List.Index
import qualified Data.Set as S
import qualified RIO.Text as T
import Utils (frequencies)

lowerBound = 246540

upperBound = 787419

solvePartOne =
  let range = [lowerBound .. upperBound]
   in length . (filter validate) $ range

validate :: Integer -> Bool
validate it =
  it < 10000000
    && it > 99999 -- six digits
    && it > lowerBound -- six digits
    && it < upperBound
    && (it & digits & isAscending)
    && (it & hasDoubleOrHigher)

hasDoubleOrHigher :: Integer -> Bool
hasDoubleOrHigher x = any (\it -> length it >= 2) $ group $ show x

solvePartTwo =
  let range = [lowerBound .. upperBound]
   in length . (filter validatePartTwo) $ range

validatePartTwo :: Integer -> Bool
validatePartTwo it =
  it < 10000000
    && it > 99999 -- six digits
    && it > lowerBound -- six digits
    && it < upperBound
    && (it & digits & isAscending)
    && (it & hasAtLeastOneDouble)

hasAtLeastOneDouble :: Integer -> Bool
hasAtLeastOneDouble x = any (\it -> length it == 2) $ group $ show x

digits :: Integer -> [Int]
digits = map digitToInt . show
