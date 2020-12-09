{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day08 where

import Control.Lens
import Data.Char
import Data.Function ((&))
import Data.List as L
import Data.List.Index
import qualified Data.List.Split as Split
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import RIO hiding (many, trace, (.~))
import System.Directory
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, separatorChar, space, string)
import qualified Text.Megaparsec.Char.Lexer as Lex
import Utils

type Parser = Parsec Void String

solvePart1 :: [String] -> Int
solvePart1 input =
  computerPart1 (parseInstructions input) 0 (0, [])

{-- Checking in the garbage that got me the answer for part 2
    will have to completely  re-write this thing because we'll
    probably use it every other day
--}
visited = [0, 1, 2, 3, 4, 447, 448, 449, 450, 357, 47, 48, 49, 50, 236, 69, 70, 71, 72, 98, 99, 100, 119, 120, 121, 213, 214, 215, 216, 172, 173, 174, 175, 176, 361, 362, 363, 383, 384, 385, 386, 616, 617, 461, 338, 574, 575, 576, 577, 589, 590, 591, 592, 593, 140, 141, 142, 143, 291, 582, 583, 584, 585, 314, 315, 613, 614, 408, 409, 410, 542, 543, 544, 545, 226, 227, 228, 229, 342, 423, 424, 425, 426, 427, 298, 299, 300, 301, 302, 374, 375, 42, 392, 393, 394, 395, 396, 283, 284, 285, 347, 348, 77, 116, 399, 400, 401, 402, 403, 233, 234, 628, 149, 150, 480, 481, 482, 483, 238, 239, 240, 265, 266, 267, 268, 269, 477, 250, 535, 536, 537, 538, 102, 103, 104, 352, 353, 63, 64, 65, 66, 67, 598, 599, 600, 601, 602, 245, 246, 247, 500, 501, 502, 503, 504, 192, 193, 194, 195, 196, 167, 276, 277, 278, 279, 280, 157, 158, 159, 160, 161, 571, 572, 329, 330, 331, 332, 333, 486, 487, 488, 489, 490, 79, 80, 29, 321, 322, 323, 13, 14, 15, 16, 17, 496, 497, 498, 200, 201, 293, 294, 133, 134, 508, 509]

indexed = zip visited $ map (inp !!) visited

indexedOnlyJmpNop = L.filter ((\it -> it `elem` ["jmp", "nop"]) . head . words . snd) $ zip visited $ map (inp !!) visited

solvePart2 :: [String] -> [Int]
solvePart2 input =
  let instructions :: [Instruction] = (parseInstructions input)
      indexedOnlyJmpNop = L.filter ((\it -> it `elem` ["jmp", "nop"]) . head . words . snd) $ zip visited $ map (inp !!) visited
      instructionsWithOneOpFlipEach :: [[Instruction]] =
        L.map (\it -> flipOneInstruction instructions it) $ (L.map fst indexedOnlyJmpNop)
   in L.map (\it -> computerPart2 it 0 (0, [])) instructionsWithOneOpFlipEach
  where
    flipOneInstruction :: [Instruction] -> Int -> [Instruction]
    flipOneInstruction instructions idx =
      (element idx .~ (flipAtIndex instructions idx)) instructions

    flipAtIndex :: [Instruction] -> Int -> Instruction
    flipAtIndex instructions idx =
      let instruction = instructions !! idx
       in ((flipOp (fst instruction)), snd instruction)

    flipOp op =
      if (op == Jmp) then Nop else Jmp

parseInstructions :: [String] -> [Instruction]
parseInstructions input =
  L.map (toInstruction . words) input
  where
    toInstruction line =
      (parseOp (head line), parseArg (last line))
    parseOp s = case s of
      "acc" -> Acc
      "jmp" -> Jmp
      "nop" -> Nop
      _ -> Err
    parseArg s =
      if head s == '+'
        then read (drop 1 s) :: Int
        else read s :: Int

type Instruction = (Operation, Argument)

data Operation = Acc | Jmp | Nop | Err deriving (Show, Eq)

type Argument = Int

type Accumulator = (Int, Visited)

type Visited = [Int]

type Cursor = Int

-- Part 1
-- This computer stops when it encounters an instruction it's visited before.
computerPart1 :: [Instruction] -> Cursor -> Accumulator -> Int
computerPart1 instructions cursor accumulator =
  if cursor `elem` (snd accumulator)
    then --(trace $ show accumulator) $
      fst accumulator
    else go $ (instructions !! cursor)
  where
    go instruction =
      case (fst instruction) of
        Acc -> computerPart1 instructions (succ cursor) ((acc instruction), visited)
        Nop -> computerPart1 instructions (succ cursor) (fst accumulator, visited)
        Jmp -> computerPart1 instructions (jump cursor instruction) (fst accumulator, visited)
    acc :: Instruction -> Int
    acc i =
      (fst accumulator) + snd i
    jump :: Cursor -> Instruction -> Cursor
    jump cursor' i =
      cursor' + (snd i)
    visited =
      (snd accumulator) ++ [cursor]

-- This computer returns -1 when it enters an infinite loop or goes out of range.
computerPart2 :: [Instruction] -> Cursor -> Accumulator -> Int
computerPart2 instructions cursor accumulator =
  if cursor `elem` (snd accumulator) || cursor >= length instructions
    then (-1)
    else
      if cursor == (subtract 1 $ length instructions)
        then (trace $ show accumulator) $ (fst accumulator)
        else go $ (instructions !! cursor)
  where
    go instruction =
      case (fst instruction) of
        Acc -> computerPart1 instructions (succ cursor) ((acc instruction), visited)
        Nop -> computerPart1 instructions (succ cursor) (fst accumulator, visited)
        Jmp -> computerPart1 instructions (jump cursor instruction) (fst accumulator, visited)

    acc :: Instruction -> Int
    acc i =
      (fst accumulator) + snd i

    jump :: Cursor -> Instruction -> Cursor
    jump cursor' i =
      cursor' + (snd i)

    visited =
      (snd accumulator) ++ [cursor]

-- -- This computer stops when it finishes all instructions
-- computer :: [Instruction] -> Cursor -> Accumulator -> Accumulator
-- computer instructions cursor accumulator =
--   if cursor >= length instructions
--   then (trace $ show accumulator) $ accumulator
--   else go $ (instructions !! cursor)
--   where
--     go instruction =
--       case (fst instruction) of
--         Acc -> computer instructions (succ cursor) ((acc instruction), visited)
--         Nop -> computer instructions (succ cursor) accumulator
--         Jmp -> computer instructions (jump cursor instruction) accumulator

--     acc :: Instruction -> Int
--     acc i =
--       (fst accumulator) + snd i

--     jump :: Cursor -> Instruction -> Cursor
--     jump cursor' i =
--       cursor' + (snd i)

--     visited =
--       (snd accumulator) ++ [cursor]

main :: IO ()
main = do
  input <- lines <$> readFile "data/2020/8.input"
  let ex1 = solvePart1 exinp1
  let answer1 = solvePart1 input
  --  let ex2 = solvePart2 exinp2
  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1

  --  putStrLn $ "Example 2: " <> show ex2
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp1 =
  [ "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -9",
    "acc +1",
    "jmp -4",
    "acc +6"
  ]

exinp2 =
  [ "nop +0",
    "acc +1",
    "jmp +4",
    "acc +3",
    "jmp -3",
    "acc -9",
    "acc +1",
    "nop -4",
    "acc +6"
  ]

inp =
  [ "jmp +1",
    "acc -15",
    "acc +14",
    "acc +18",
    "jmp +443",
    "jmp +286",
    "acc +27",
    "jmp +522",
    "jmp +1",
    "acc -19",
    "acc +22",
    "acc +37",
    "jmp +111",
    "acc +28",
    "acc +43",
    "acc +18",
    "nop +597",
    "jmp +479",
    "jmp +604",
    "jmp +499",
    "acc +0",
    "acc +22",
    "acc +13",
    "jmp +566",
    "acc -12",
    "acc +0",
    "nop +153",
    "jmp +173",
    "jmp +192",
    "jmp +292",
    "acc +36",
    "acc +7",
    "jmp +440",
    "acc -17",
    "acc +40",
    "acc +24",
    "acc -7",
    "jmp +519",
    "nop +16",
    "acc +15",
    "acc +42",
    "jmp +445",
    "jmp +350",
    "acc +42",
    "acc +12",
    "acc +2",
    "jmp +133",
    "acc +12",
    "acc +3",
    "acc +27",
    "jmp +186",
    "acc +25",
    "acc +46",
    "jmp +285",
    "acc +32",
    "acc -11",
    "acc -6",
    "jmp +565",
    "nop +215",
    "acc +1",
    "acc +35",
    "jmp +1",
    "jmp +502",
    "acc +27",
    "acc +19",
    "acc -8",
    "acc -8",
    "jmp +531",
    "jmp -21",
    "nop +292",
    "acc +8",
    "acc -13",
    "jmp +26",
    "acc +1",
    "acc +45",
    "nop -42",
    "jmp +323",
    "jmp +39",
    "jmp +336",
    "acc +19",
    "jmp -51",
    "acc +45",
    "acc +26",
    "jmp +278",
    "jmp +6",
    "acc +40",
    "nop +271",
    "acc -10",
    "nop -4",
    "jmp +272",
    "nop -61",
    "acc +4",
    "acc -14",
    "acc +27",
    "jmp -70",
    "acc -9",
    "acc +29",
    "jmp +416",
    "acc +25",
    "acc +45",
    "jmp +19",
    "jmp +39",
    "acc -19",
    "acc +7",
    "jmp +248",
    "acc +11",
    "acc +36",
    "jmp +515",
    "acc +45",
    "acc +49",
    "jmp +329",
    "acc +30",
    "acc +31",
    "acc +28",
    "acc +26",
    "jmp +8",
    "jmp +283",
    "acc +32",
    "jmp +127",
    "acc +4",
    "acc +20",
    "jmp +92",
    "jmp +50",
    "jmp +133",
    "acc +5",
    "acc +8",
    "jmp +313",
    "acc +38",
    "acc +34",
    "jmp +395",
    "acc +14",
    "acc +29",
    "jmp +392",
    "nop +246",
    "jmp +374",
    "nop +429",
    "nop +388",
    "acc +3",
    "acc +0",
    "jmp +432",
    "acc -1",
    "acc +35",
    "acc +35",
    "jmp +148",
    "acc +8",
    "acc +11",
    "acc +12",
    "acc -10",
    "jmp +434",
    "acc -19",
    "jmp +330",
    "nop +329",
    "acc +30",
    "jmp +239",
    "acc -6",
    "jmp -136",
    "jmp +418",
    "nop +385",
    "jmp +1",
    "acc +34",
    "acc +9",
    "jmp +410",
    "nop -13",
    "acc +31",
    "acc +15",
    "acc +37",
    "jmp -142",
    "jmp +109",
    "acc -16",
    "nop +405",
    "nop +343",
    "jmp +8",
    "acc +44",
    "acc -15",
    "acc +7",
    "acc +9",
    "jmp +185",
    "acc +6",
    "jmp +35",
    "nop -25",
    "jmp +93",
    "acc +22",
    "acc -17",
    "acc +15",
    "acc +39",
    "jmp +41",
    "nop -123",
    "acc +15",
    "acc +6",
    "jmp -35",
    "acc +48",
    "jmp +422",
    "acc -7",
    "nop +67",
    "nop +66",
    "acc +48",
    "jmp -29",
    "acc -11",
    "acc +16",
    "jmp +92",
    "acc +45",
    "jmp +92",
    "jmp +212",
    "acc -3",
    "acc -18",
    "nop -186",
    "nop +7",
    "jmp -28",
    "nop +292",
    "acc +7",
    "nop -120",
    "acc +46",
    "jmp +48",
    "acc -3",
    "acc -16",
    "acc +50",
    "jmp -44",
    "acc -2",
    "acc -11",
    "jmp +236",
    "jmp +344",
    "acc +33",
    "acc +44",
    "acc +39",
    "nop -45",
    "jmp -53",
    "acc -11",
    "nop +380",
    "acc +35",
    "jmp +113",
    "nop +203",
    "acc +40",
    "jmp +167",
    "acc +44",
    "jmp +394",
    "jmp +229",
    "jmp -167",
    "jmp -204",
    "acc +21",
    "acc +49",
    "jmp +25",
    "acc -19",
    "acc -17",
    "acc +44",
    "jmp -11",
    "acc +40",
    "acc +12",
    "jmp +253",
    "acc +21",
    "jmp +349",
    "jmp +285",
    "acc +0",
    "nop +261",
    "acc +15",
    "acc +38",
    "jmp +10",
    "acc +27",
    "jmp +1",
    "jmp +373",
    "jmp -151",
    "acc +6",
    "jmp -48",
    "acc +14",
    "acc -8",
    "jmp -61",
    "acc +8",
    "acc +20",
    "jmp +1",
    "jmp +1",
    "jmp +208",
    "acc -18",
    "acc +32",
    "jmp +94",
    "jmp +262",
    "acc +0",
    "jmp -156",
    "nop +188",
    "nop +312",
    "acc +21",
    "acc +6",
    "jmp -123",
    "acc +47",
    "jmp +316",
    "acc +25",
    "nop +290",
    "jmp +62",
    "acc -7",
    "acc +36",
    "nop +212",
    "acc +14",
    "jmp +332",
    "jmp +291",
    "jmp +226",
    "acc +30",
    "jmp -161",
    "acc +39",
    "acc +38",
    "jmp +203",
    "nop +63",
    "nop -6",
    "acc -15",
    "nop -56",
    "jmp +72",
    "acc +1",
    "acc +34",
    "acc +22",
    "acc +19",
    "jmp -135",
    "acc +27",
    "jmp -303",
    "acc +1",
    "acc +48",
    "acc -19",
    "jmp +142",
    "acc +50",
    "jmp +298",
    "acc +43",
    "acc +0",
    "acc +50",
    "acc +12",
    "jmp +137",
    "acc +41",
    "nop +252",
    "jmp -310",
    "acc +13",
    "acc +34",
    "acc -15",
    "acc +43",
    "jmp +236",
    "acc +5",
    "acc -8",
    "acc +25",
    "acc +45",
    "jmp +153",
    "acc -12",
    "acc +31",
    "acc -1",
    "jmp +120",
    "jmp +236",
    "acc +38",
    "nop -238",
    "jmp -328",
    "jmp +81",
    "acc +48",
    "acc +15",
    "acc -9",
    "jmp -73",
    "nop -49",
    "jmp -271",
    "acc -17",
    "acc -17",
    "jmp +106",
    "nop +212",
    "jmp -290",
    "acc +36",
    "nop +109",
    "jmp +186",
    "jmp -310",
    "acc +4",
    "acc +16",
    "jmp +117",
    "jmp +1",
    "acc +10",
    "jmp +20",
    "acc +12",
    "jmp -311",
    "acc +12",
    "acc +30",
    "nop +182",
    "jmp -315",
    "acc +25",
    "acc +12",
    "acc +30",
    "jmp +50",
    "acc -19",
    "jmp -333",
    "acc +30",
    "nop +87",
    "jmp -199",
    "acc +8",
    "jmp +112",
    "acc -8",
    "jmp -313",
    "acc +7",
    "acc +32",
    "jmp +1",
    "jmp +230",
    "acc +25",
    "acc +45",
    "acc +20",
    "acc +0",
    "jmp -307",
    "acc +30",
    "nop -253",
    "acc +7",
    "acc +39",
    "jmp -113",
    "acc -12",
    "jmp +209",
    "acc +42",
    "acc +17",
    "acc -19",
    "acc +24",
    "jmp -170",
    "acc +30",
    "acc +9",
    "acc -1",
    "jmp -328",
    "acc +19",
    "acc +45",
    "jmp +132",
    "nop -244",
    "nop +35",
    "jmp +34",
    "acc -10",
    "acc +26",
    "acc +35",
    "nop -238",
    "jmp +54",
    "acc +15",
    "nop -378",
    "acc +42",
    "jmp -43",
    "acc -9",
    "acc -5",
    "acc -11",
    "nop -307",
    "jmp -129",
    "nop -202",
    "acc -9",
    "nop -376",
    "acc +11",
    "jmp -75",
    "jmp +14",
    "acc -1",
    "acc +32",
    "acc -14",
    "acc +16",
    "jmp +39",
    "acc +42",
    "acc +32",
    "jmp -133",
    "acc +1",
    "acc +17",
    "nop +85",
    "acc +35",
    "jmp +83",
    "acc +27",
    "acc +0",
    "acc -12",
    "jmp -93",
    "acc +48",
    "acc +35",
    "nop +154",
    "jmp -287",
    "jmp -347",
    "jmp -348",
    "acc +18",
    "jmp -374",
    "acc -15",
    "jmp +36",
    "jmp -123",
    "acc -11",
    "jmp +55",
    "acc +19",
    "acc +23",
    "jmp -339",
    "nop +5",
    "acc +44",
    "acc +2",
    "jmp +1",
    "jmp -417",
    "acc +23",
    "jmp -253",
    "acc -9",
    "acc -3",
    "jmp -138",
    "jmp -227",
    "acc +12",
    "jmp -437",
    "acc +47",
    "acc +19",
    "acc -6",
    "jmp -245",
    "acc +2",
    "jmp -328",
    "acc -14",
    "acc +25",
    "acc +4",
    "acc -2",
    "jmp -411",
    "jmp -351",
    "jmp -459",
    "acc +3",
    "acc +48",
    "jmp -134",
    "nop +54",
    "acc -14",
    "jmp -298",
    "jmp -401",
    "acc -14",
    "acc +25",
    "nop -55",
    "acc -10",
    "jmp -312",
    "acc -7",
    "acc +45",
    "jmp -74",
    "acc +30",
    "jmp -462",
    "acc +5",
    "acc -8",
    "jmp -355",
    "acc +9",
    "acc +44",
    "acc +44",
    "jmp -150",
    "jmp -484",
    "acc +14",
    "acc +19",
    "acc -6",
    "jmp -474",
    "acc -18",
    "jmp -166",
    "jmp -264",
    "acc -15",
    "acc +17",
    "acc +29",
    "jmp -149",
    "nop -273",
    "acc +31",
    "acc +0",
    "acc -2",
    "jmp -410",
    "jmp -411",
    "acc +47",
    "acc -6",
    "nop -287",
    "jmp -436",
    "acc +4",
    "nop +88",
    "jmp -158",
    "acc +32",
    "jmp +1",
    "acc -15",
    "jmp -319",
    "acc -6",
    "acc -18",
    "acc +49",
    "jmp -256",
    "acc -18",
    "acc +31",
    "acc +27",
    "acc +27",
    "jmp -351",
    "jmp +58",
    "acc +12",
    "jmp +1",
    "acc +32",
    "nop -151",
    "jmp -411",
    "acc +19",
    "acc +7",
    "jmp -287",
    "acc +30",
    "jmp -496",
    "acc -11",
    "acc +5",
    "acc +42",
    "acc +25",
    "jmp -249",
    "acc -1",
    "jmp -243",
    "jmp -190",
    "acc +32",
    "acc +32",
    "acc +14",
    "jmp +12",
    "acc +5",
    "acc +30",
    "acc +34",
    "jmp -46",
    "acc -13",
    "acc +5",
    "acc +45",
    "jmp -271",
    "acc +29",
    "acc +37",
    "jmp -323",
    "nop -18",
    "acc -2",
    "acc +21",
    "acc -12",
    "jmp -453",
    "acc -14",
    "acc +19",
    "nop -173",
    "jmp -411",
    "acc +24",
    "acc -7",
    "nop -136",
    "acc +6",
    "jmp -357",
    "acc -1",
    "acc -1",
    "acc +32",
    "jmp -264",
    "acc +26",
    "jmp -175",
    "acc +10",
    "acc +35",
    "nop -361",
    "jmp -493",
    "acc +14",
    "jmp -206",
    "jmp -138",
    "acc -1",
    "jmp -156",
    "acc +3",
    "acc +11",
    "acc -2",
    "jmp -213",
    "acc +35",
    "acc -13",
    "acc +47",
    "acc +45",
    "jmp -376",
    "jmp -543",
    "jmp -479",
    "acc +29",
    "jmp -532",
    "acc +28",
    "acc +47",
    "acc -11",
    "acc -14",
    "jmp +1"
  ]
