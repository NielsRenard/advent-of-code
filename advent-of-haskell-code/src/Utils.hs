{-# LANGUAGE NoImplicitPrelude #-}

module Utils
  ( frequencies,
  )
where

import RIO
import qualified RIO.List as L
import qualified RIO.Map as M

frequencies :: (Ord k, Num a) => [k] -> [(k, a)]
frequencies xs = M.toList $ M.fromListWith (+) [(c, 1) | c <- xs]
