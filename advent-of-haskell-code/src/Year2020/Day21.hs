{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Year2020.Day21
  (
  )
where

import Data.Char
import Data.List as L
import Data.List.Utils
import Data.List.Split as Split hiding (sepBy)
import qualified Data.Set as S
import Data.List.Index
import Data.Maybe
import Data.Text as T (unpack)
import NeatInterpolation (text)
import Debug.Trace (trace)
import System.Directory
import Data.Void
import qualified Data.HashMap.Strict as HM
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

type Ingredient = String
type Allergen = String
type Food = ([Ingredient], [Allergen])

{- part 1
-}

-- runParser foodParser "" "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
-- Right (["mxmxvkd","kfcds","sqjhc","nhms",""],["dairy","fish"])
foodParser :: Parser Food
foodParser = do
  ingredients <- many lowerChar `sepEndBy` spaceChar
  parensOpen  <- char '(' 
  contains    <- string "contains"
  space
  allergens   <- many alphaNumChar `sepBy` (string ", ")
  parensClose <- char ')'
  return $ (L.filter (/= "") ingredients, allergens) -- filter 'cos I keep parsing "" at end of ingredients
  
parseInput input =
  mapMaybe (parseMaybe foodParser) $ lines input  

getFoodsByIngredient :: [Food] -> Ingredient -> [Food]
getFoodsByIngredient allFood ingredient =
  mapMaybe (\food ->
              if (((ingredient `elem`) . fst) food )
              then (Just food)
              else Nothing)
  allFood

getFoodsByAllergen :: [Food] -> Allergen -> [Food]
getFoodsByAllergen allFood allergen =
  mapMaybe (\food ->
              if allergen `elem` (snd food)
              then (Just food)
              else Nothing
         ) allFood

getFoodsByAllergen2 :: [Food] -> Allergen -> (Allergen, [Food])
getFoodsByAllergen2 allFood allergen =
  (allergen, foods)
  where
    foods = 
      mapMaybe (\food ->
                  if allergen `elem` (snd food)
                  then (Just food)
                  else Nothing
               ) allFood


possibleAllergens :: [Food] -> Ingredient -> (Ingredient, S.Set Allergen)
possibleAllergens allFood ingredient =
  (ingredient, (S.fromList $ concatMap snd $ getFoodsByIngredient allFood ingredient))

solvePart1 :: String -> Int
solvePart1 input =
  let
    allFood            = parseInput input
    allIngredients    :: [Ingredient] = concatMap fst allFood    
    allAllergens      :: S.Set Allergen   = S.fromList $ concatMap snd allFood
    allIngredientsByAllergen :: S.Set [S.Set Ingredient] =
      S.map ((map (S.fromList . fst)) . getFoodsByAllergen allFood) allAllergens
  in
    sum . map (\ingredient -> countElem ingredient allIngredients) $ -- count occurrences
    S.toList $
    S.difference (S.fromList allIngredients) $ --  'safe' ingredients      (do not contain allergens)
    S.unions $                                 --  'dangerous' ingredients (contain allergens)
    S.map (\allergen ->
             foldl' S.intersection (S.fromList allIngredients) allergen)
    allIngredientsByAllergen

--solvePart2 :: String -> Int
solvePart2 input =
  let
    allFood            = parseInput input
    allIngredients    :: [Ingredient] = concatMap fst allFood    
    allAllergens      :: [Allergen]   = concatMap snd allFood
    allFoodsByAllergen :: [(Allergen, [Food])] = map (getFoodsByAllergen2 allFood) (nub allAllergens)
  in
    map (\(allergen, foods) ->
            let
              ingredients = map S.fromList $ (map fst foods)
            in
             (allergen, (foldl' S.intersection (S.fromList allIngredients) ingredients)))
    allFoodsByAllergen

{- part 2 last bit manual (Same process of elimination as day 16)
  ("dairy",     ["lmzg"]
  ("fish",      ["cxk"])
  ("nuts",      ["bsqh"])
  ("peanuts",   ["bdvmx"])
  ("sesame",    ["cpbzbx"])
  ("shellfish", ["drbm"])
  ("soy",       ["cfnt"])
  ("wheat",     ["kqprv"])
-}

main = do
  input <- readFile "data/2020/21.input"
  let ex1 = solvePart1 exinp
  let answer1 = solvePart1 input
  let ex2 = solvePart2 exinp
  let answer2 = solvePart2 input
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "   Part 1: " <> show answer1
  putStrLn $ "Example 2: " <> show ex2
  putStrLn $ "   Part 2: " <> show answer2

{-- Test and example input --}

exinp :: String
exinp = T.unpack $
  [text|
       mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
       trh fvjkl sbzzf mxmxvkd (contains dairy)
       sqjhc fvjkl (contains soy)
       sqjhc mxmxvkd sbzzf (contains fish)
       |]

food         = parseInput exinp
ingredients  = concatMap fst food    
allergens    = concatMap snd food

    
