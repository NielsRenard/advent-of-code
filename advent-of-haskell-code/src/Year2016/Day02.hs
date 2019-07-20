{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day02
  ()
where

import           Data.Char
import           Data.Maybe
import           Prelude                        ( last
                                                , print
                                                , read
                                                , toEnum
                                                )
import           RIO                     hiding ( many )
import           RIO.List                      as L
import           RIO.List.Partial              as L'
import           RIO.Text                      as T
import           Text.ParserCombinators.ReadP


answerOne = L.map face solvePartOne

-- Lot of duplication here, and fixed amount of digits. Think of something smart with fold
solvePartOne :: [Button]
solvePartOne =
  [firstButton, secondButton, thirdButton, fourthButton, fifthButton]
 where
  firstButton  = L.foldl translate startButton $ head allInstructions
  secondButton = L.foldl translate firstButton $ allInstructions !! 1
  thirdButton  = L.foldl translate secondButton $ allInstructions !! 2
  fourthButton = L.foldl translate thirdButton $ allInstructions !! 3
  fifthButton  = L.foldl translate thirdButton $ allInstructions !! 4

data Button = Button {x :: Int, y :: Int, face :: Int} deriving (Show)
data Step = StepLeft | StepRight | StepUp | StepDown deriving (Show)
startButton = Button { x = 0, y = 0, face = 5 }

translate :: Button -> Step -> Button
translate b s =
  let x' = x b
      y' = y b
  in  case s of
        StepLeft  -> pad b (x' - 1) y'
        StepRight -> pad b (x' + 1) y'
        StepUp    -> pad b x' (y' + 1)
        StepDown  -> pad b x' (y' - 1)

parseString :: Text -> [Step]
-- this fst . last . is to just take the final result, there is smoother way
parseString s = fst . last . readP_to_S instruction $ T.unpack s

instruction :: ReadP [Step]
instruction = many1 step

step :: ReadP Step
step = do
  t' <- char 'L' <|> char 'R' <|> char 'U' <|> char 'D'
  return
    (case t' of
      'L' -> StepLeft
      'R' -> StepRight
      'U' -> StepUp
      'D' -> StepDown
    )

pad :: Button -> Int -> Int -> Button
pad b x y | x == -1, y == 1  = Button { x = x, y = y, face = 1 }
          | x == 0, y == 1   = Button { x = x, y = y, face = 2 }
          | x == 1, y == 1   = Button { x = x, y = y, face = 3 }
          | x == -1, y == 0  = Button { x = x, y = y, face = 4 }
          | x == 0, y == 0   = Button { x = x, y = y, face = 5 }
          | x == 1, y == 0   = Button { x = x, y = y, face = 6 }
          | x == -1, y == -1 = Button { x = x, y = y, face = 7 }
          | x == 0, y == -1  = Button { x = x, y = y, face = 8 }
          | x == 1, y == -1  = Button { x = x, y = y, face = 9 }
          | otherwise        = b

allInstructions = L.map parseString input

exampleInput = L.map T.pack ["ULL", "RRDDD", "LURDL", "UUUUD"]

-- puzzle input
input = L.map
  T.pack
  [ "DLDRDDDLULDRRLUDDLDUURDRDUULDRDDRRLDLLUUDDLLRLRDRUURLUDURDDRURLUDDUULUURLLRRRRUDULUDLULLUURRLLRRURRUDUUURRLUUUDURDLLLDULDRLRDDDUDDUURLRRRURULLUDDUULDRRRDDLRLUDDRRDLRDURLRURUDDUULDDUUDDURRLUURRULRRLDLULLRLRUULDUDDLLLRDDULRUDURRDUUDUUDDUULULURDLUDRURDLUUDRDUURDDDRDRLDLDRURRLLRURURLLULLRRUULRRRRDLDULDDLRRRULRURRDURUDUUULDUUDRLDDLDUDDRULLUDUULRRRDRRDRDULDLURDDURLRUDLURLUDDDRLLURUUUUUUURUULDUUDDRLULRUDURRDLDUULLRLULLURDDDDDLRRDLRLLDDUDRRRDDURDLRRUDDUDLRRRDDURULRURRRLDRDUDLD"
  , "LRRDUDUUUDRRURRDUUULULUDDLLDRRRUDDUULRRDRUDRLLRLRULRRDUUDRLDURUDLLLDRRDLRLUUDRUDRRRUDRRRULDRRLLRDDDLLRDDRULRLLRUDRLLLULDLDDRDRUUUUUULURLLRUDRDRLLULLRUUURRDRULULUDLDURRUUDURLLUDRDLDDULUDLRDDRLRLURULDRURRRRURRDDUDRULUUUDDDRULRULDLLURUUULRDDLRUURLRLDLUULLURDRDDDUDDDRLDRDLLDRDDDDURLUUULDDRURULUDDURDRDRLULDULURDUURDRLLUUUULRULUUDRLLDDRRURUURLDLLRRRDLRURDDLDLDRLRRDLDURULDDLULRRRUUDLRDUURDURLURDDLDLRURLLLDRDULDDRUDDULDDRRLDLRDRDLDUUDLUULRLUDUUDUUUULDURULRRUDULURLRLDRLULLLDUDLLLRUDURDDDURLDDLRLRRDLUDLDDDDLULDRLDUUULDRRDDLRUULDLULUUURUDDRLDDDULRUDRURUURUUURRULRURDURLLRLLUULUULURDRLLUDDLU"
  , "LLDURDUDRLURUDRLRLUDDRRURDULULDDUDUULRRLRLRRDRDRDURRLRLURRLRUDULLUULLURUDDRLDDDRURLUUDLDURRDURDDLUULRDURRUUURLRRURRDRDRDURRRLULLDRUDLRUDURDRDDLLULLULRRUDULDDRDRRDLLLDLURLRDRDLUDDRLDDLDRULDURLLRLDRDLUDDDDLDUUDRLLRRRRLDDRRLRLURLLRLLUULLDUUDLRDRRRDRDLLDULLDRLDDUDRDDRURRDDLRDLRRUUDRRRRDURUULDRDDURLURRRRURRDRRULULURULUUUDRRRLDLLLDDRULRUDDURDRLDDRDLULLLRURUDRLRDDLDLRRRUURDURLDURRUUDDLRDRUUUURDLRLULRUUDRLDLULLULUURURDULUDUDRRRLLRLURLLDLRRURURRUDLUDDDDRDUDUDUUUULLDRDLLLLUUUUDRLRLUDURLLUDRUUDLLURUULDDDDULUUURLLDL"
  , "DLULLRDLRRLLLDLRRURRDRURDRUUULDDRLURURRDLRRULUUDDRLRRLDULRRUUDUULDDDUDLLDLURDRLLULLUUULLDURDRRRDDLRDUDRRRLRLDRRLRLULDDUDURRRLDLRULDULDDUDDRULDLDRDRDDRUDRUDURRRRUUDUDRLDURLDLRRUURRDDUDLLDUDRRURRLRRRRRLDUDDRLLLURUDRRUDRLRDUDUUUUUDURULLDUUDLRUUULDUUURURLUUDULDURUDDDLRRRDDRRDLRULLLRDDRLRLUULDUUULLLLDLRURLRRDURRLDLLLDURDLLUDDDLLDDURDDULURDRRRDDDLDDURRULUUDDLULLURULUULDLDDLUDRURURULUDDULRDRLDRRRUUUURUULDRLRRURRLULULURLLDRLRLURULRDDDULRDDLUR"
  , "RURRULLRRDLDUDDRRULUDLURLRRDDRDULLLUUDDDRDDRRULLLDRLRUULRRUDLDLLLRLLULDRLDDDLLDDULLDRLULUUUURRRLLDRLDLDLDDLUDULRDDLLRLLLULLUDDRDDUUUUDLDLRRDDRDLUDURRUURUURDULLLLLULRRLDRLRDLUURDUUDLDRURURLLDRRRLLLLRDLDURRLRRLLRUUDDUULLRLUDLRRRRRURUDDURULURRUULRDDULUUDUUDDRDDDDDUUUDDDRRLDDRRDDUUULDURLDULURDRDLLURDULRUDRUULUULLRRRRLRUUDDUDLDURURLRRRULRDRRUDDRDDRLRRRLRURRRUULULLLUULLLULLUDLRDLDURRURDLDLRDUULDRLLRRLDUDDUULULR"
  ]
