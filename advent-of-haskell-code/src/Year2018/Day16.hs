module Main where

main = print "hey"


data Instruction = Instruction{ opcode :: Opcode, inputs :: (Int, Int) , output :: Int}

data Opcode = Int

readInts :: String -> [Int]
readInts = map (read . filter (/= '+')) . lines
