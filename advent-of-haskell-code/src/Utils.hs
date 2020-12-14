{-# LANGUAGE NoImplicitPrelude #-}

module Utils
  ( takeWhileInclusive,
    frequencies,
    digits,
    digitsToInt,
    atLeastAtMost,
    stringToInts,
    binaryToDecimal,
    fromBinary,
    toBinary,
    leftPadZeroes
  )
where

import Data.Char
import Data.List as L
import RIO
import qualified RIO.Map as M
import qualified RIO.Text as T
import Prelude (read)

takeWhileInclusive :: Eq a => [a] -> (a -> Bool) -> [a]
takeWhileInclusive xs pred =
  let (upTo, rest) = span pred xs
   in upTo ++ [(head rest)]

atLeastAtMost :: Integer -> Integer -> Integer -> Bool
atLeastAtMost l h number = l <= number && number <= h

frequencies :: (Ord k, Num a) => [k] -> [(k, a)]
frequencies xs = M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]

-- conversion

leftPadZeroes :: Int -> [Int] -> [Int]
leftPadZeroes desiredLength xs =
  replicate (desiredLength - length ys) 0 ++ ys
  where ys = take desiredLength xs

toBinary :: Int -> [ Int ]
toBinary 0 = []
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

fromBinary :: [Int] -> Int
fromBinary = binaryToDecimal

binaryToDecimal :: [Int] -> Int
binaryToDecimal binaryList =
  convert (reverse binaryList) 0
  where
    convert [] _ = 0
    convert (x : xs) n = (x * 2 ^ n) + convert xs (n + 1)

stringToInts :: Text -> [Int]
stringToInts =
  (map digitToInt) . T.unpack

-- be aware these functions take off leading zeros --
-- [1,2,3] to [123]
digits :: Int -> [Int]
digits i = map (read . return) . show $ i

-- [1,2,3] to [123]
-- [0,1,2,3] to [123] ?
digitsToInt :: [Integer] -> Integer
digitsToInt = read . concatMap show

-----------------------------------------------------
