{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day01 () where

import RIO
import Prelude (print)
import qualified RIO.ByteString as B

main :: IO ()
main = do
    contents <- B.readFile "data/2016/input/input_2016_01_a.txt"
    let contents' = show $ B.length contents
    print contents
    print contents'
