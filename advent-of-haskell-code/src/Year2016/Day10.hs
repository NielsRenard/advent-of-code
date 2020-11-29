{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE  RecordWildCards#-}


module Year2016.Day10
  (
  )
where

import RIO hiding (try)
import qualified RIO.List as L
import qualified RIO.Text as T
import Data.Maybe (fromJust, mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char (char,  string, alphaNumChar )
import qualified Text.Megaparsec.Char.Lexer as Lex
import qualified Data.Set as Set

import Prelude
  ( head,
    last,
    read,
    repeat,
    max
  )

-- ✓ value 5 goes to bot 2 => 0:[], 1:[], 2:[5] 
-- bot 2 gives low to bot 1 and high to bot 0 => 0:[], 1:[], 2:[5]
-- ✓ value 3 goes to bot 1  => 0:[], 1:[3], 2:[5]
-- bot 1 gives low to output 1 and high to bot 0 => 0:[], 1:[3], 2:[5]
-- bot 0 gives low to output 2 and high to output 0 => 0:[], 1:[3], 2:[5]
-- ✓ value 2 goes to bot 2  => 0:[], 1:[3], 2:[2,5]
-- round 2 -----------------------------------------------------------------------
-- ✓ bot 2 gives low to bot 1 and high to bot 0 => 0:[5], 1:[2,3], 2:[]
-- ✓ bot 1 gives low to output 1 and high to bot 0 => 0:[3,5], 1:[], 2:[5] output1:[2]
-- ✓ bot 0 gives low to output 2 and high to output 0 => 0:[], 1:[3], 2:[5] output2:[3] output0:[5]

data Bot
  = Bot { botId :: Int,
          low :: Maybe Int,
          high :: Maybe Int
        } deriving (Show)

data Output
  = Output { outputId :: Int,
             values :: [Int]
           } deriving (Show)


data Instruction
  = BotInstruction
      { bot :: Int,
        givesLowTo :: Int,
        lowReceiver :: BotOrOutputReceives,
        givesHighTo :: Int,
        highReceiver :: BotOrOutputReceives
      }
  | ValuePass
      { value :: Int,
        receiver :: Int
      }
  deriving (Show)

data BotOrOutputReceives = BotReceives | OutputReceives deriving (Enum, Show)

type Parser = Parsec Void Text

botGivesParser :: Parser Instruction
botGivesParser = do
  bot <- string "bot"
  space <- char ' '
  id <- Lex.decimal
  space <- char ' '
  givesLow <- string "gives low to"
  space <- char ' '
  lowToBotOrOutput <- choice [ string "bot", string "output"]  
  space <- char ' '
  low <- Lex.decimal
  space <- char ' '  
  andHigh <- string "and high to"
  space <- char ' '
  highToBotOrOutput  <- choice [ string "bot", string "output"]  
  space <- char ' '
  high <- Lex.decimal
  case (lowToBotOrOutput, highToBotOrOutput) of
    ("bot","bot") ->
      return $ BotInstruction id low BotReceives high BotReceives  
    ("bot","output") ->
      return $ BotInstruction id low BotReceives high OutputReceives  
    ("output","bot") ->
      return $ BotInstruction id low OutputReceives high BotReceives  
    ("output","output") ->
      return $ BotInstruction id low OutputReceives high OutputReceives    

valuePassParser :: Parser Instruction
valuePassParser = do
  valueString <- string "value"
  space <- char ' '
  value <- Lex.decimal
  space <- char ' '
  goesTo <- string "goes to"
  space <- char ' '
  botId <- string "bot"
  space <- char ' '  
  botOrOutputId <- Lex.decimal
  return $
    ValuePass value botOrOutputId

{- repl
   bot0 = Bot 0 Nothing Nothing
   bot1 = Bot 1 Nothing Nothing
   bot2 = Bot 2 Nothing Nothing
   value5GoesToBot2 = fromJust $ parseMaybe (valuePassParser) $ T.pack "value 5 goes to bot 2"
   bots = executeValueInstruction [bot0, bot1, bot2] value5GoesToBot2

   bot2Gives = fromJust $ parseMaybe (botGivesParser) $ T.pack "bot 2 gives low to bot 1 and high to bot 0"
   executeBotInstruction (bots, []) bot2Gives


   L.map (fromRight (Bot 0 0 0))  $ L.map (runParser botGivesParser "whatever") $ L.filter (\line -> not(T.isPrefixOf "value" line)) input
   L.map (runParser botGivesParser "notImportant") $ input
   L.map (runParser botGivesParser "whatever") $ L.filter (\line -> not(T.isPrefixOf "value" line)) input
   parseTest botGivesParser $ T.pack "bot 88 gives low to bot 51 and high to bot 42"
   parseTest botGivesParser $ T.pack "bot 57 gives low to output 16 and high to bot 3"
   parseTest valuePassParser $ T.pack "value 67 goes to bot 187"
   parseTest (satisfy (== 'bot') :: botGivesParser) $ T.pack "ba-"
-}
  
executeBotInstruction :: ([Bot], [Output]) -> Instruction -> ([Bot],[Output])
executeBotInstruction (bots, outputs) instruction =
  let
    giver = fromJust $ L.find (\it -> botId it == bot instruction) bots
    updatedBotsAndOutputs =
      case (lowReceiver instruction, highReceiver instruction) of
        (BotReceives, BotReceives) ->
          ((L.map (\bot' ->
                      -- low
                      if (botId bot') == (givesLowTo instruction)
                      then giveBotValue bot' (fromJust $ takeLowValue giver)
                      else
                        -- high
                        if (botId bot') == (givesHighTo instruction)
                        then giveBotValue bot' (fromJust $ takeHighValue giver)
                        else bot'
                  )
             bots),
            outputs)
          
        (BotReceives, OutputReceives) ->
          ((L.map (\bot' ->
                      -- low
                      if (botId bot') == (givesLowTo instruction)
                      then giveBotValue bot' (fromJust $ takeLowValue giver)
                      else bot'
                  )
             bots),
            (L.map (\output ->
                      if outputId output == givesHighTo instruction
                      then giveOutputValue output (fromJust $ takeHighValue giver)
                      else output
                        )
              outputs))
        (OutputReceives, BotReceives) ->
          ((L.map (\bot' ->
                      -- low
                      if (botId bot') == (givesHighTo instruction)
                      then giveBotValue bot' (fromJust $ takeHighValue giver)
                      else bot'
                  )
             bots),
            (L.map (\output ->
                      if outputId output == givesLowTo instruction
                      then giveOutputValue output (fromJust $ takeLowValue giver)
                      else output
                        )
              outputs))
        (OutputReceives, OutputReceives) ->
          (bots,
           (L.map (\output' ->
                      -- low
                      if (outputId output') == (givesLowTo instruction)
                      then giveOutputValue output' (fromJust $ takeLowValue giver)
                      else
                        -- high
                        if (outputId output') == (givesHighTo instruction)
                        then giveOutputValue output' (fromJust $ takeHighValue giver)
                        else output'
                  )
             outputs))
  in
    updatedBotsAndOutputs
  
-- should refactor Bot to just have a tuple, instead of specific low and high
takeLowValue :: Bot -> Maybe Int
takeLowValue bot =
  case low bot of
    Just l -> Just l
    Nothing -> case high bot of
                 Just h -> Just h
                 Nothing -> Nothing

-- should refactor Bot to just have a tuple, instead of specific low and high
takeHighValue :: Bot -> Maybe Int
takeHighValue bot =
  case high bot of
    Just h -> Just h
    Nothing -> case low bot of
                 Just l -> Just l
                 Nothing -> Nothing

giveOutputValue :: Output -> Int -> Output
giveOutputValue output value =
  let newValues = (values output) ++ [value]
  in
    output { values = newValues } 
    

giveBotValue :: Bot -> Int -> Bot
giveBotValue bot value =
  let botLow = low bot
      botHigh = high bot
  in
    if botLow /= Nothing
    then if Just value < botLow
         then
           bot { low = Just value, high = botLow }
         else
           bot { low = botLow, high = Just value }
    else
      if Just value < botHigh
         then
           bot { low = Just value, high = botHigh }
         else
           bot { low = botHigh, high = Just value }
    
executeValueInstruction :: [Bot] -> Instruction -> [Bot]
executeValueInstruction bots valuePass =
    map (\bot ->
           if botId bot == (receiver valuePass)
           then
             case (low bot, high bot) of
               (Just _, Just _) -> -- hands are full
                 bot
               (Nothing, Just _) ->
                 let lower = min (low bot) (Just (value valuePass))
                     higher = max (low bot) (Just (value valuePass))
                 in
                 bot { low =  lower
                     , high =  higher}
               (Just _, Nothing) ->
                 let lower = min (low bot) (Just (value valuePass))
                     higher = max (low bot) (Just (value valuePass))
                 in                 
                 bot { low = lower,
                       high =  higher}
               (Nothing, Nothing) -> -- doesn't matter which field we use
                 bot { low =  Just (value valuePass) }
           else
             bot
        )
         bots



parseInstructions :: [Text] -> [Instruction]
parseInstructions input =
  mapMaybe (parseMaybe (try botGivesParser <|> valuePassParser)) input

allBots :: [Instruction] -> [Bot]
allBots instructions =
  let
    allBotInstructions = L.filter (\it -> case it of BotInstruction _ _ _ _ _ -> True; ValuePass _ _ -> False) instructions  
    allBotIds = L.map bot allBotInstructions
  in
    L.map (\it -> (Bot it Nothing Nothing)) allBotIds

allOutputs :: [Instruction] -> [Output]
allOutputs instructions =
  let
    allBotInstructions = L.filter (\it -> case it of
                                      BotInstruction _ _ OutputReceives _ _ -> True
                                      BotInstruction _ _ _ _ OutputReceives -> True
                                      BotInstruction _ _ _ _ _ -> False
                                      ValuePass _ _ -> False) instructions  
    outputsThatReceiveLow = L.map givesLowTo allBotInstructions
    outputsThatReceiveHigh = L.map givesHighTo allBotInstructions
    distinctOutputIds = Set.toList . Set.fromList $ (outputsThatReceiveLow ++ outputsThatReceiveHigh)
  in
    L.map (\outputId -> Output outputId [] ) distinctOutputIds
  

executeInstructions :: ([Bot], [Output]) -> [Instruction] -> ([Bot], [Output])
executeInstructions (bots, outputs) instructions =
  let
    instruction = head instructions
  in
    if trace ("bots: " <> (T.pack $ show bots)) $ L.length instructions > 0
    then 
      case instruction of
        BotInstruction bot' lowTo lowBotOrOutput highTo highBotOrOutput ->
          -- bot x gives low to y and high to z
          -- only proceed when bot has two chips
          let
            giver = fromJust $ L.find (\it -> botId it == bot instruction) bots
          in
            if (low giver) == Nothing || (high giver) == Nothing
            then
              -- trace ("skipping bot instruction for giver: " <> (T.pack $ show giver)
              --        <> (T.pack $ show instruction)) $ 
            executeInstructions (bots, outputs) $ L.drop 1 instructions ++ [instruction]              
            else
              -- trace ("performing bot instruction for giver: " <> (T.pack $ show giver) <> (T.pack $ show instruction)) $ 
              executeInstructions (executeBotInstruction (bots, outputs) instruction) $ L.drop 1 instructions
        ValuePass val receiver ->
          -- value x goes to bot y
          -- trace "valuepass instruction" 
          executeInstructions ((executeValueInstruction bots instruction), outputs) $ L.drop 1 instructions
      else
      (bots, outputs)

{- repl example
 exampleInstructions = parseInstructions exampleInput
 botsList= allBots exampleInstructions
 outputsList= allOutputs exampleInstructions
 executeInstructions (botsList, outputsList) exampleInstructions
-}

{- repl real input
 instructions = parseInstructions input
 botsList= allBots instructions
 outputsList= allOutputs instructions
 executeInstructions (botsList, outputsList) instructions

 then I manually went through the trace logging to find
 Bot {botId = 27, low = Just 17, high = Just 61}
 (Could easily filter here too).

  TODO: Refactor so bots don't have lower/higher fields, but rather two fields, calculating which one is higher only when running instructions.
-}

exampleInput =
  L.map
    T.pack
    [ "value 5 goes to bot 2",                                   
      "bot 2 gives low to bot 1 and high to bot 0",
      "value 3 goes to bot 1",
      "bot 1 gives low to output 1 and high to bot 0",
      "bot 0 gives low to output 2 and high to output 0",
      "value 2 goes to bot 2"
    ]

input :: [Text]
input =
  L.map
    T.pack
    [ "bot 88 gives low to bot 51 and high to bot 42",
      "bot 13 gives low to bot 4 and high to bot 167",
      "bot 90 gives low to bot 78 and high to bot 199",
      "bot 84 gives low to bot 205 and high to bot 201",
      "bot 41 gives low to bot 48 and high to bot 15",
      "bot 15 gives low to bot 156 and high to bot 54",
      "bot 70 gives low to output 10 and high to bot 4",
      "bot 140 gives low to bot 206 and high to bot 189",
      "value 67 goes to bot 187",
      "bot 124 gives low to bot 99 and high to bot 102",
      "bot 203 gives low to bot 55 and high to bot 33",
      "bot 118 gives low to bot 131 and high to bot 79",
      "bot 78 gives low to bot 15 and high to bot 122",
      "bot 82 gives low to bot 112 and high to bot 127",
      "bot 207 gives low to bot 136 and high to bot 50",
      "bot 57 gives low to output 16 and high to bot 3",
      "bot 101 gives low to bot 165 and high to bot 209",
      "bot 60 gives low to output 12 and high to bot 5",
      "bot 31 gives low to bot 104 and high to bot 44",
      "bot 104 gives low to bot 115 and high to bot 35",
      "value 31 goes to bot 97",
      "bot 117 gives low to bot 0 and high to bot 85",
      "bot 175 gives low to bot 74 and high to bot 92",
      "bot 160 gives low to bot 116 and high to bot 69",
      "bot 96 gives low to bot 107 and high to bot 8",
      "bot 55 gives low to bot 13 and high to bot 126",
      "bot 36 gives low to bot 140 and high to bot 91",
      "bot 111 gives low to bot 63 and high to bot 131",
      "bot 159 gives low to bot 59 and high to bot 30",
      "bot 105 gives low to bot 132 and high to bot 34",
      "value 29 goes to bot 86",
      "bot 187 gives low to bot 40 and high to bot 87",
      "bot 191 gives low to bot 148 and high to bot 39",
      "bot 73 gives low to bot 105 and high to bot 20",
      "bot 194 gives low to output 7 and high to bot 107",
      "bot 62 gives low to bot 61 and high to bot 55",
      "bot 89 gives low to bot 36 and high to bot 141",
      "bot 170 gives low to bot 1 and high to bot 139",
      "bot 51 gives low to bot 147 and high to bot 113",
      "bot 108 gives low to bot 92 and high to bot 162",
      "bot 162 gives low to bot 180 and high to bot 68",
      "bot 0 gives low to bot 77 and high to bot 152",
      "bot 112 gives low to bot 33 and high to bot 127",
      "bot 52 gives low to bot 101 and high to bot 49",
      "bot 71 gives low to bot 169 and high to bot 41",
      "bot 195 gives low to output 4 and high to bot 66",
      "bot 93 gives low to bot 17 and high to bot 123",
      "bot 192 gives low to bot 47 and high to bot 124",
      "bot 209 gives low to bot 188 and high to bot 16",
      "bot 49 gives low to bot 209 and high to bot 154",
      "bot 99 gives low to output 19 and high to bot 142",
      "bot 97 gives low to bot 32 and high to bot 129",
      "bot 198 gives low to bot 110 and high to bot 67",
      "value 73 goes to bot 12",
      "bot 20 gives low to bot 34 and high to bot 137",
      "bot 50 gives low to bot 153 and high to bot 0",
      "bot 171 gives low to bot 67 and high to bot 146",
      "bot 138 gives low to bot 100 and high to bot 74",
      "bot 106 gives low to bot 83 and high to bot 82",
      "bot 151 gives low to bot 197 and high to bot 101",
      "value 47 goes to bot 36",
      "bot 26 gives low to bot 98 and high to bot 104",
      "bot 134 gives low to bot 149 and high to bot 143",
      "value 3 goes to bot 29",
      "bot 7 gives low to bot 30 and high to bot 110",
      "value 43 goes to bot 206",
      "bot 150 gives low to output 11 and high to bot 53",
      "bot 44 gives low to bot 35 and high to bot 18",
      "bot 127 gives low to bot 134 and high to bot 143",
      "bot 74 gives low to bot 157 and high to bot 192",
      "bot 129 gives low to bot 37 and high to bot 158",
      "bot 17 gives low to bot 27 and high to bot 73",
      "bot 196 gives low to bot 208 and high to bot 83",
      "value 71 goes to bot 89",
      "bot 81 gives low to bot 181 and high to bot 71",
      "bot 2 gives low to bot 12 and high to bot 45",
      "bot 176 gives low to bot 191 and high to bot 172",
      "value 59 goes to bot 207",
      "bot 76 gives low to bot 41 and high to bot 78",
      "value 41 goes to bot 136",
      "bot 66 gives low to output 15 and high to bot 100",
      "bot 115 gives low to bot 163 and high to bot 10",
      "bot 141 gives low to bot 91 and high to bot 119",
      "bot 46 gives low to bot 172 and high to bot 98",
      "bot 158 gives low to bot 93 and high to bot 123",
      "bot 4 gives low to output 14 and high to bot 194",
      "bot 33 gives low to bot 126 and high to bot 134",
      "bot 83 gives low to bot 184 and high to bot 82",
      "bot 137 gives low to bot 31 and high to bot 44",
      "bot 25 gives low to bot 141 and high to bot 22",
      "bot 131 gives low to bot 135 and high to bot 128",
      "value 37 goes to bot 43",
      "bot 135 gives low to bot 162 and high to bot 130",
      "bot 120 gives low to bot 88 and high to bot 178",
      "bot 189 gives low to bot 114 and high to bot 176",
      "bot 130 gives low to bot 68 and high to bot 208",
      "bot 64 gives low to bot 85 and high to bot 59",
      "value 13 goes to bot 103",
      "bot 1 gives low to bot 202 and high to bot 185",
      "value 2 goes to bot 57",
      "bot 197 gives low to bot 57 and high to bot 165",
      "bot 163 gives low to bot 7 and high to bot 198",
      "bot 22 gives low to bot 119 and high to bot 132",
      "bot 8 gives low to bot 94 and high to bot 173",
      "bot 152 gives low to bot 65 and high to bot 133",
      "value 19 goes to bot 151",
      "bot 167 gives low to bot 194 and high to bot 96",
      "bot 116 gives low to bot 79 and high to bot 69",
      "bot 193 gives low to bot 38 and high to bot 200",
      "bot 80 gives low to bot 183 and high to bot 9",
      "bot 114 gives low to bot 161 and high to bot 191",
      "bot 113 gives low to bot 125 and high to bot 1",
      "bot 32 gives low to bot 103 and high to bot 129",
      "bot 136 gives low to bot 86 and high to bot 153",
      "bot 182 gives low to bot 45 and high to bot 17",
      "bot 18 gives low to bot 171 and high to bot 146",
      "bot 155 gives low to bot 46 and high to bot 26",
      "bot 148 gives low to bot 201 and high to bot 168",
      "bot 54 gives low to bot 175 and high to bot 108",
      "bot 201 gives low to bot 117 and high to bot 64",
      "bot 174 gives low to bot 139 and high to bot 160",
      "bot 103 gives low to bot 187 and high to bot 37",
      "bot 28 gives low to bot 9 and high to bot 147",
      "bot 132 gives low to bot 155 and high to bot 23",
      "bot 34 gives low to bot 23 and high to bot 137",
      "bot 39 gives low to bot 168 and high to bot 58",
      "bot 161 gives low to bot 84 and high to bot 148",
      "bot 98 gives low to bot 109 and high to bot 115",
      "bot 199 gives low to bot 122 and high to bot 111",
      "bot 12 gives low to bot 89 and high to bot 25",
      "bot 188 gives low to bot 150 and high to bot 21",
      "bot 9 gives low to bot 76 and high to bot 90",
      "bot 27 gives low to bot 22 and high to bot 105",
      "bot 23 gives low to bot 26 and high to bot 31",
      "value 61 goes to bot 2",
      "bot 154 gives low to bot 16 and high to bot 81",
      "bot 157 gives low to output 8 and high to bot 47",
      "bot 109 gives low to bot 58 and high to bot 163",
      "bot 75 gives low to bot 178 and high to bot 56",
      "bot 156 gives low to bot 138 and high to bot 175",
      "bot 95 gives low to bot 5 and high to bot 166",
      "bot 11 gives low to bot 176 and high to bot 46",
      "bot 91 gives low to bot 189 and high to bot 11",
      "bot 86 gives low to bot 151 and high to bot 52",
      "bot 149 gives low to bot 96 and high to bot 145",
      "bot 173 gives low to bot 60 and high to bot 95",
      "bot 67 gives low to bot 120 and high to bot 75",
      "value 5 goes to bot 140",
      "value 11 goes to bot 121",
      "bot 100 gives low to output 13 and high to bot 157",
      "value 53 goes to bot 197",
      "bot 186 gives low to bot 170 and high to bot 174",
      "bot 185 gives low to bot 118 and high to bot 116",
      "bot 19 gives low to bot 195 and high to bot 190",
      "bot 126 gives low to bot 167 and high to bot 149",
      "bot 143 gives low to bot 145 and high to bot 204",
      "bot 40 gives low to bot 2 and high to bot 182",
      "bot 122 gives low to bot 54 and high to bot 63",
      "bot 16 gives low to bot 21 and high to bot 181",
      "bot 110 gives low to bot 177 and high to bot 120",
      "bot 208 gives low to bot 72 and high to bot 184",
      "bot 205 gives low to bot 50 and high to bot 117",
      "bot 168 gives low to bot 64 and high to bot 159",
      "bot 29 gives low to bot 43 and high to bot 84",
      "bot 79 gives low to bot 128 and high to bot 24",
      "value 23 goes to bot 97",
      "bot 94 gives low to output 5 and high to bot 60",
      "bot 178 gives low to bot 42 and high to bot 186",
      "bot 200 gives low to bot 28 and high to bot 51",
      "bot 63 gives low to bot 108 and high to bot 135",
      "bot 5 gives low to output 17 and high to bot 166",
      "bot 30 gives low to bot 193 and high to bot 177",
      "bot 47 gives low to output 3 and high to bot 99",
      "bot 24 gives low to bot 196 and high to bot 106",
      "bot 128 gives low to bot 130 and high to bot 196",
      "bot 3 gives low to output 6 and high to bot 150",
      "bot 77 gives low to bot 49 and high to bot 65",
      "bot 61 gives low to bot 70 and high to bot 13",
      "bot 190 gives low to bot 66 and high to bot 138",
      "bot 142 gives low to output 2 and high to bot 70",
      "bot 87 gives low to bot 182 and high to bot 93",
      "bot 121 gives low to bot 29 and high to bot 161",
      "bot 204 gives low to bot 173 and high to bot 95",
      "bot 139 gives low to bot 185 and high to bot 160",
      "bot 35 gives low to bot 10 and high to bot 18",
      "bot 37 gives low to bot 87 and high to bot 158",
      "bot 179 gives low to bot 81 and high to bot 183",
      "bot 10 gives low to bot 198 and high to bot 171",
      "bot 38 gives low to bot 80 and high to bot 28",
      "bot 92 gives low to bot 192 and high to bot 180",
      "bot 177 gives low to bot 200 and high to bot 88",
      "bot 166 gives low to output 20 and high to output 9",
      "bot 42 gives low to bot 113 and high to bot 170",
      "bot 14 gives low to bot 133 and high to bot 38",
      "bot 172 gives low to bot 39 and high to bot 109",
      "bot 165 gives low to bot 3 and high to bot 188",
      "bot 169 gives low to bot 19 and high to bot 48",
      "bot 125 gives low to bot 199 and high to bot 202",
      "bot 85 gives low to bot 152 and high to bot 14",
      "bot 102 gives low to bot 142 and high to bot 61",
      "bot 180 gives low to bot 124 and high to bot 144",
      "value 7 goes to bot 40",
      "bot 68 gives low to bot 144 and high to bot 72",
      "bot 72 gives low to bot 62 and high to bot 203",
      "bot 56 gives low to bot 186 and high to bot 174",
      "bot 184 gives low to bot 203 and high to bot 112",
      "bot 65 gives low to bot 154 and high to bot 179",
      "bot 58 gives low to bot 159 and high to bot 7",
      "bot 147 gives low to bot 90 and high to bot 125",
      "bot 206 gives low to bot 121 and high to bot 114",
      "bot 202 gives low to bot 111 and high to bot 118",
      "bot 21 gives low to bot 53 and high to bot 164",
      "bot 153 gives low to bot 52 and high to bot 77",
      "bot 119 gives low to bot 11 and high to bot 155",
      "bot 164 gives low to bot 6 and high to bot 19",
      "bot 146 gives low to bot 75 and high to bot 56",
      "bot 123 gives low to bot 73 and high to bot 20",
      "bot 145 gives low to bot 8 and high to bot 204",
      "bot 183 gives low to bot 71 and high to bot 76",
      "bot 69 gives low to bot 24 and high to bot 106",
      "bot 6 gives low to output 18 and high to bot 195",
      "bot 144 gives low to bot 102 and high to bot 62",
      "bot 43 gives low to bot 207 and high to bot 205",
      "bot 59 gives low to bot 14 and high to bot 193",
      "value 17 goes to bot 32",
      "bot 181 gives low to bot 164 and high to bot 169",
      "bot 107 gives low to output 1 and high to bot 94",
      "bot 45 gives low to bot 25 and high to bot 27",
      "bot 133 gives low to bot 179 and high to bot 80",
      "bot 53 gives low to output 0 and high to bot 6",
      "bot 48 gives low to bot 190 and high to bot 156"
    ]

