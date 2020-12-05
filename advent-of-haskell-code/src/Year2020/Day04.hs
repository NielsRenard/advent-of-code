{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Year2020.Day04 where

import Data.Char
import Data.Function ((&))
import Data.List as L
import Data.List.Index
import Data.List.Split (splitWhen)
import Data.Maybe
import Data.String
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text as T
import Debug.Trace (trace)
import System.Directory
import Utils

type Passport = [(Key, Value)]

type Key = Text

type Value = Text

data Part = PartOne | PartTwo deriving (Eq)

solve :: Part -> [Text] -> Int
solve part input =
  let requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
      -- optionalFields = ["cid"] -- not using this (Will we need it on the way back from our vacation?)
      passports :: [Passport] = parsePassports input
      passportsContainingRequiredFields =
        filter ((== True) . (containsKeys requiredFields)) passports
   in if part == PartOne
        then length $ passportsContainingRequiredFields
        else length $ filter fieldsAreValid $ passportsContainingRequiredFields

{-- Part one --}

containsKeys :: [Key] -> Passport -> Bool
containsKeys keys passport =
  and $ map (`elem` (map fst passport)) keys

{-- Part Two  --}

fieldsAreValid :: Passport -> Bool
fieldsAreValid passport =
  and $
    [ validByr passport,
      validIyr passport,
      validEyr passport,
      validHgt passport,
      validHcl passport,
      validEcl passport,
      validPid passport
    ]

-- | unsafeLookup should not be called on Passports missing required fields, as it assumes the key is present. --}
unsafeLookup :: Passport -> Key -> String
unsafeLookup passport key =
  (fromJust $ T.unpack <$> lookup key passport)

validByr :: Passport -> Bool
validByr passport = atLeastAtMost 1920 2002 (read (unsafeLookup passport "byr") :: Integer)

validIyr :: Passport -> Bool
validIyr passport = atLeastAtMost 2010 2020 (read (unsafeLookup passport "iyr") :: Integer)

validEyr :: Passport -> Bool
validEyr passport = atLeastAtMost 2020 2030 (read (unsafeLookup passport "eyr") :: Integer)

validHgt :: Passport -> Bool
validHgt passport =
  let number = read (takeWhile isDigit (unsafeLookup passport "hgt")) :: Integer
      unit = dropWhile isDigit (unsafeLookup passport "hgt")
   in if unit == "cm" then atLeastAtMost 150 193 number else atLeastAtMost 59 76 number

validHcl :: Passport -> Bool
validHcl passport =
  let hairColour = unsafeLookup passport "hcl"
      colourCode = (drop 1 hairColour)
   in head hairColour == '#'
        && (length colourCode == 6)
        && (and $ map isAlphaNum colourCode)

validEcl :: Passport -> Bool
validEcl passport = unsafeLookup passport "ecl" `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: Passport -> Bool
validPid passport = length (unsafeLookup passport "pid") == 9

parsePassports :: [Text] -> [Passport]
parsePassports input = (map (map keyValue) . map getFields) $ splitWhen (== "") input
  where
    getFields :: [Text] -> [Text]
    getFields = concatMap (T.splitOn " ")
    keyValue :: Text -> (Key, Value)
    keyValue field =
      ((\[k, v] -> (k, v)) . T.splitOn ":") field

main :: IO ()
main = do
  input <- map T.pack <$> lines <$> readFile "data/2020/4.input"
  let answer1 = solve PartOne input
  let answer2 = solve PartTwo input
  putStrLn $ "Part 1: " <> show answer1
  putStrLn $ "Part 2: " <> show answer2

{-- Test and example input --}

exinp :: [Text]
exinp =
  [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
    "byr:1937 iyr:2017 cid:147 hgt:183cm",
    "",
    "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
    "hcl:#cfa07d byr:1929",
    "",
    "hcl:#ae17e1 iyr:2013",
    "eyr:2024",
    "ecl:brn pid:760753108 byr:1931",
    "hgt:179cm",
    "",
    "hcl:#cfa07d eyr:2025 pid:166559648",
    "iyr:2011 ecl:brn hgt:59in"
  ]

invalidPassportsInput :: [Text]
invalidPassportsInput =
  [ "eyr:1972 cid:100",
    "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
    "",
    "iyr:2019",
    "hcl:#602927 eyr:1967 hgt:170cm",
    "ecl:grn pid:012533040 byr:1946",
    "",
    "hcl:dab227 iyr:2012",
    "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
    "",
    "hgt:59cm ecl:zzz",
    "eyr:2038 hcl:74454a iyr:2023",
    "pid:3556412378 byr:2007"
  ]

validPassportsInput :: [Text]
validPassportsInput =
  [ "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
    "hcl:#623a2f",
    "",
    "eyr:2029 ecl:blu cid:129 byr:1989",
    "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
    "",
    "hcl:#888785",
    "hgt:164cm byr:2001 iyr:2015 cid:88",
    "pid:545766238 ecl:hzl",
    "eyr:2022",
    "",
    "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
  ]
