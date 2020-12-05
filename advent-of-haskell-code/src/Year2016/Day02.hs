{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day02
  (
  )
where

import RIO
import RIO.List as L
import RIO.Text as T
import Text.ParserCombinators.ReadP
import Prelude
  ( head,
    last,
    (!!),
  )

answerOne = L.map face solvePartOne

answerTwo = L.map face solvePartTwo

-- Lot of duplication here, and fixed amount of digits. Think of something smart with fold
solvePartOne :: [Button]
solvePartOne =
  [firstButton, secondButton, thirdButton, fourthButton, fifthButton]
  where
    firstButton = L.foldl translate startButton $ head allInstructions
    secondButton = L.foldl translate firstButton $ allInstructions !! 1
    thirdButton = L.foldl translate secondButton $ allInstructions !! 2
    fourthButton = L.foldl translate thirdButton $ allInstructions !! 3
    fifthButton = L.foldl translate thirdButton $ allInstructions !! 4

solvePartTwo :: [Button]
solvePartTwo =
  [firstButton, secondButton, thirdButton, fourthButton, fifthButton]
  where
    firstButton = L.foldl translate2 startButton2 $ head allInstructions
    secondButton = L.foldl translate2 firstButton $ allInstructions !! 1
    thirdButton = L.foldl translate2 secondButton $ allInstructions !! 2
    fourthButton = L.foldl translate2 thirdButton $ allInstructions !! 3
    fifthButton = L.foldl translate2 thirdButton $ allInstructions !! 4

data Button = Button {x :: Int, y :: Int, face :: Char} deriving (Show)

data Step = StepLeft | StepRight | StepUp | StepDown deriving (Show)

startButton = Button {x = 0, y = 0, face = '5'}

startButton2 = Button {x = -2, y = 0, face = '5'}

translate :: Button -> Step -> Button
translate b s =
  let x' = x b
      y' = y b
   in case s of
        StepLeft -> pad b (x' - 1) y'
        StepRight -> pad b (x' + 1) y'
        StepUp -> pad b x' (y' + 1)
        StepDown -> pad b x' (y' - 1)

translate2 :: Button -> Step -> Button
translate2 b s =
  let x' = x b
      y' = y b
   in case s of
        StepLeft -> pad2 b (x' - 1) y'
        StepRight -> pad2 b (x' + 1) y'
        StepUp -> pad2 b x' (y' + 1)
        StepDown -> pad2 b x' (y' - 1)

parseString :: Text -> [Step]
-- this fst . last . is to just take the final result, there is smoother way
parseString s = fst . last . readP_to_S instruction $ T.unpack s

instruction :: ReadP [Step]
instruction = many1 step

step :: ReadP Step
step = do
  t' <- char 'L' <|> char 'R' <|> char 'U' <|> char 'D'
  return
    ( case t' of
        'L' -> StepLeft
        'R' -> StepRight
        'U' -> StepUp
        'D' -> StepDown
    )

pad :: Button -> Int -> Int -> Button
pad b x y
  | x == -1, y == 1 = Button {x = x, y = y, face = '1'}
  | x == 0, y == 1 = Button {x = x, y = y, face = '2'}
  | x == 1, y == 1 = Button {x = x, y = y, face = '3'}
  | x == -1, y == 0 = Button {x = x, y = y, face = '4'}
  | x == 0, y == 0 = Button {x = x, y = y, face = '5'}
  | x == 1, y == 0 = Button {x = x, y = y, face = '6'}
  | x == -1, y == -1 = Button {x = x, y = y, face = '7'}
  | x == 0, y == -1 = Button {x = x, y = y, face = '8'}
  | x == 1, y == -1 = Button {x = x, y = y, face = '9'}
  | otherwise = b

pad2 :: Button -> Int -> Int -> Button
pad2 b x y
  | x == 0, y == 2 = Button {x = x, y = y, face = '1'}
  | x == -1, y == 1 = Button {x = x, y = y, face = '2'}
  | x == 0, y == 1 = Button {x = x, y = y, face = '3'}
  | x == 1, y == 1 = Button {x = x, y = y, face = '4'}
  | x == -2, y == 0 = Button {x = x, y = y, face = '5'}
  | x == -1, y == 0 = Button {x = x, y = y, face = '6'}
  | x == 0, y == 0 = Button {x = x, y = y, face = '7'}
  | x == 1, y == 0 = Button {x = x, y = y, face = '8'}
  | x == 2, y == 0 = Button {x = x, y = y, face = '9'}
  | x == -1, y == -1 = Button {x = x, y = y, face = 'A'}
  | x == 0, y == -1 = Button {x = x, y = y, face = 'B'}
  | x == 1, y == -1 = Button {x = x, y = y, face = 'C'}
  | x == 0, y == -2 = Button {x = x, y = y, face = 'D'}
  | otherwise = b

allInstructions = L.map parseString input

exampleInput = L.map T.pack ["ULL", "RRDDD", "LURDL", "UUUUD"]

-- puzzle input
input =
  L.map
    T.pack
    [ "DLDRDDDLULDRRLUDDLDUURDRDUULDRDDRRLDLLUUDDLLRLRDRUURLUDURDDRURLUDDUULUURLLRRRRUDULUDLULLUURRLLRRURRUDUUURRLUUUDURDLLLDULDRLRDDDUDDUURLRRRURULLUDDUULDRRRDDLRLUDDRRDLRDURLRURUDDUULDDUUDDURRLUURRULRRLDLULLRLRUULDUDDLLLRDDULRUDURRDUUDUUDDUULULURDLUDRURDLUUDRDUURDDDRDRLDLDRURRLLRURURLLULLRRUULRRRRDLDULDDLRRRULRURRDURUDUUULDUUDRLDDLDUDDRULLUDUULRRRDRRDRDULDLURDDURLRUDLURLUDDDRLLURUUUUUUURUULDUUDDRLULRUDURRDLDUULLRLULLURDDDDDLRRDLRLLDDUDRRRDDURDLRRUDDUDLRRRDDURULRURRRLDRDUDLD",
      "LRRDUDUUUDRRURRDUUULULUDDLLDRRRUDDUULRRDRUDRLLRLRULRRDUUDRLDURUDLLLDRRDLRLUUDRUDRRRUDRRRULDRRLLRDDDLLRDDRULRLLRUDRLLLULDLDDRDRUUUUUULURLLRUDRDRLLULLRUUURRDRULULUDLDURRUUDURLLUDRDLDDULUDLRDDRLRLURULDRURRRRURRDDUDRULUUUDDDRULRULDLLURUUULRDDLRUURLRLDLUULLURDRDDDUDDDRLDRDLLDRDDDDURLUUULDDRURULUDDURDRDRLULDULURDUURDRLLUUUULRULUUDRLLDDRRURUURLDLLRRRDLRURDDLDLDRLRRDLDURULDDLULRRRUUDLRDUURDURLURDDLDLRURLLLDRDULDDRUDDULDDRRLDLRDRDLDUUDLUULRLUDUUDUUUULDURULRRUDULURLRLDRLULLLDUDLLLRUDURDDDURLDDLRLRRDLUDLDDDDLULDRLDUUULDRRDDLRUULDLULUUURUDDRLDDDULRUDRURUURUUURRULRURDURLLRLLUULUULURDRLLUDDLU",
      "LLDURDUDRLURUDRLRLUDDRRURDULULDDUDUULRRLRLRRDRDRDURRLRLURRLRUDULLUULLURUDDRLDDDRURLUUDLDURRDURDDLUULRDURRUUURLRRURRDRDRDURRRLULLDRUDLRUDURDRDDLLULLULRRUDULDDRDRRDLLLDLURLRDRDLUDDRLDDLDRULDURLLRLDRDLUDDDDLDUUDRLLRRRRLDDRRLRLURLLRLLUULLDUUDLRDRRRDRDLLDULLDRLDDUDRDDRURRDDLRDLRRUUDRRRRDURUULDRDDURLURRRRURRDRRULULURULUUUDRRRLDLLLDDRULRUDDURDRLDDRDLULLLRURUDRLRDDLDLRRRUURDURLDURRUUDDLRDRUUUURDLRLULRUUDRLDLULLULUURURDULUDUDRRRLLRLURLLDLRRURURRUDLUDDDDRDUDUDUUUULLDRDLLLLUUUUDRLRLUDURLLUDRUUDLLURUULDDDDULUUURLLDL",
      "DLULLRDLRRLLLDLRRURRDRURDRUUULDDRLURURRDLRRULUUDDRLRRLDULRRUUDUULDDDUDLLDLURDRLLULLUUULLDURDRRRDDLRDUDRRRLRLDRRLRLULDDUDURRRLDLRULDULDDUDDRULDLDRDRDDRUDRUDURRRRUUDUDRLDURLDLRRUURRDDUDLLDUDRRURRLRRRRRLDUDDRLLLURUDRRUDRLRDUDUUUUUDURULLDUUDLRUUULDUUURURLUUDULDURUDDDLRRRDDRRDLRULLLRDDRLRLUULDUUULLLLDLRURLRRDURRLDLLLDURDLLUDDDLLDDURDDULURDRRRDDDLDDURRULUUDDLULLURULUULDLDDLUDRURURULUDDULRDRLDRRRUUUURUULDRLRRURRLULULURLLDRLRLURULRDDDULRDDLUR",
      "RURRULLRRDLDUDDRRULUDLURLRRDDRDULLLUUDDDRDDRRULLLDRLRUULRRUDLDLLLRLLULDRLDDDLLDDULLDRLULUUUURRRLLDRLDLDLDDLUDULRDDLLRLLLULLUDDRDDUUUUDLDLRRDDRDLUDURRUURUURDULLLLLULRRLDRLRDLUURDUUDLDRURURLLDRRRLLLLRDLDURRLRRLLRUUDDUULLRLUDLRRRRRURUDDURULURRUULRDDULUUDUUDDRDDDDDUUUDDDRRLDDRRDDUUULDURLDULURDRDLLURDULRUDRUULUULLRRRRLRUUDDUDLDURURLRRRULRDRRUDDRDDRLRRRLRURRRUULULLLUULLLULLUDLRDLDURRURDLDLRDUULDRLLRRLDUDDUULULR"
    ]
