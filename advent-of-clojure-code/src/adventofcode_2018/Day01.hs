module Main where

main :: IO()
main = do
  input <- (readFile "../../resources/2018/input_01.txt")
  let answer = sum $ readInts input
  print answer

readInts :: String -> [Int]
readInts = map (read . filter (/= '+')) . lines
