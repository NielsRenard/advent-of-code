{-# LANGUAGE NoImplicitPrelude #-}

module Utils
  ( frequencies,
    digits,
    digitsToInt
  )
where

import RIO
import qualified RIO.List as L
import qualified RIO.Map as M
import Prelude (read)

frequencies :: (Ord k, Num a) => [k] -> [(k, a)]
frequencies xs = M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]


-- be aware these functions take off leading zeros -- 
digits :: Int -> [Int]                             -- 
digits i = map (read . return) . show $ i          -- 
                                                   -- 
digitsToInt :: [Integer] -> Integer                -- 
digitsToInt = read . concatMap show                --
-----------------------------------------------------
