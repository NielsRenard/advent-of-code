{-# LANGUAGE NoImplicitPrelude #-}

module Utils
  ( frequencies
  )
where

import qualified RIO.Map                       as M
import           RIO
import qualified RIO.List                      as L

frequencies :: (Ord k, Num a) => [k] -> [(k, a)]
frequencies xs = M.toList $ M.fromListWith (+) [ (c, 1) | c <- xs ]
