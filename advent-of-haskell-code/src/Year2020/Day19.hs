{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Year2020.Day19
  (
  )
where

import Data.List as L
import Data.Tuple
import Data.Tuple
import qualified Data.Text as T
import Data.Tuple.Utils
import Debug.Trace (trace)
import qualified Data.List.Split as Split
import NeatInterpolation (text)
import Data.Bifunctor
import Data.Maybe
import System.Directory
import Data.Ord (comparing)
import Text.Megaparsec
import Text.Megaparsec.Char (upperChar, alphaNumChar, char, separatorChar, space, string, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Void

type Parser = Parsec Void String

{- part 1
  
-}

type Id = Int
data Rule = Literal Id Char | MultipleRules Id [Int] | OrRules Id ([Int], [Int]) deriving (Show)

rulesParser :: Parser Rule
rulesParser = do
  try ruleParserLiteral
   <|> try ruleParserOr
   <|> try ruleParserMultipleRules

-- parseMaybe ruleParserMultipleRules "2: 2 3 3"
-- => Just (MultipleRules 2 [2,3,3])
ruleParserMultipleRules :: Parser Rule
ruleParserMultipleRules = do
  ruleNumber <- decimal
  colon <- char ':'
  space
  rules <- decimal `sepBy` space
  return $ MultipleRules ruleNumber rules 

-- parseMaybe ruleParserLiteral "0: \"a\""
-- => Just (Literal 0 'a')
ruleParserLiteral :: Parser Rule
ruleParserLiteral = do
  ruleNumber <- decimal
  colon <- char ':'
  space
  quotes <- char '"'
  character <- char 'a' <|> char 'b'
  quotes <- char '"'  
  return $ Literal ruleNumber character

-- parseMaybe ruleParserOr "0: 1 2 | 3 4"
-- => Just (OrRules 0 ([1,2],[3,4]))
ruleParserOr :: Parser Rule
ruleParserOr = do
  ruleNumber <- decimal
  colon <- char ':'
  space
  leftRule1 <- decimal
  space
  leftRule2 <- decimal
  space
  pipe <- char '|'
  space
  rightRules <- decimal `sepBy` space -- not sure why I'm not able to use this on left side of pipe
  return $ OrRules ruleNumber ([leftRule1, leftRule2], rightRules)
  

--solvePart1 :: String -> [String]
solvePart1 input =
  let [ruleStrings, messages] = Split.splitWhen (== "") $ lines input
      rules = catMaybes $ map (parseMaybe rulesParser) ruleStrings
  in
--    (trace $ show rules) $
    map (\msg -> (chomp msg rules 0)) messages

--chomp :: String -> [Rule] -> Int -> String
chomp [] allRules ruleId  = []
-- chomp msg allRules ruleId  =
--   case (allRules !! ruleId)  of
--     Literal _ letter ->
--       if ([letter] `isPrefixOf` msg)
--       then tail msg
--       else []
--     MultipleRules _ rs ->
--       checkMultipleRules msg allRules rs
--     OrRules _ (rs1, rs2) ->
--       nub $ concat [checkMultipleRules msg allRules rs1, checkMultipleRules msg allRules rs2]


-- checkMultipleRules msg allRules [] =  string
-- checkMultipleRules msg allRules (rId:rIds)  =
--   let remainingString = (chomp msg allRules rId)
--   in
--     if (null remainingString)
--     then []
--     else checkMultipleRules remainingString allRules rIds
  

main = do
  input <- readFile "data/2020/19.input"
  let ex1 = solvePart1 exinp
--  let answer1 = length $ solvePart1 input
--  let ex2 = solvePart2 exinp2
--  let answer2 = solvePart2 input
--  putStrLn $ "Example 1: " <> (show ex1)
--  putStrLn $ "   Part 1: " <> show answer1
--  putStrLn $ "Example 2: " <> show ex2
--  putStrLn $ "   Part 2: " <> show answer2


{-- Test and example input --}


exinp :: String
exinp = T.unpack $
  [text|
       0: 4 1 5
       1: 2 3 | 3 2
       2: 4 4 | 5 5
       3: 4 5 | 5 4
       4: "a"
       5: "b"
       
       ababbb
       bababa
       abbbab
       aaabbb
       aaaabbb
|]

--exinp :: String
--exinp = [trimming |
--          0: 4 1 5
--          1: 2 3 | 3 2
--          2: 4 4 | 5 5
--          3: 4 5 | 5 4
--          4: "a"
--          5: "b"
--
--          ababbb
--          bababa
--          abbbab
--          aaabbb
--          aaaabbb|]
