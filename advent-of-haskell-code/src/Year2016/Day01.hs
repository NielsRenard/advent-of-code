{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2016.Day01 () where

import RIO
import RIO.Text (pack)
import System.IO (hPutStrLn, stderr)
import qualified RIO.ByteString as B

main :: IO ()
main = do
    contents <- B.readFile "data/2016/input/input_2016_01_a.txt"
    B.putStr contents
