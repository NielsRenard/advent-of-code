module Year2019.Day04 where

import Data.List
import Data.List.HT
import Data.List.Index
import Data.Char
import qualified Data.Set as S
import qualified RIO.Text as T
import Data.Function ((&))
import Utils (frequencies)


--solvePartOne :: Integral a => [a]
solvePartOne =
  let range = [lowerBound..upperBound]
  in
    length . (filter validate) $ range

validate :: Integer -> Bool
validate it = it < 10000000 &&   -- six digits 
              it > 99999 &&      -- six digits
              it > lowerBound &&
              it < upperBound &&
              (it & digits & isAscending) &&
              (it & hasAtLeastOneDouble)

lowerBound = 246540
upperBound = 787419

hasAtLeastOneDouble :: Integer -> Bool
hasAtLeastOneDouble xs = 
  let password = digits xs in
    (length password > 1) &&
    (head password == (password !! 1)
     || hasAtLeastOneDouble (read (concatMap show (tail password)) :: Integer))

digits :: Integer -> [Int]
digits = map digitToInt . show
