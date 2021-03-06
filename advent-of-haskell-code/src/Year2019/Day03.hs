{-# LANGUAGE ScopedTypeVariables #-}

module Year2019.Day03 where

import Data.List as L
import Data.List.Index
import qualified Data.Set as S
import qualified RIO.Text as T
import Utils (frequencies)

solvePartOne = solveOne wire1 wire2

-- * Year2019.Day03> solvePartOne

-- (1.17 secs, 390,856,352 bytes)

solvePartTwo = solveTwo wire1 wire2

-- * Year2019.Day03> solvePartTwo

-- 35038
-- (7.17 secs, 8,075,884,016 bytes) ...yikes

solveOne w1 w2 =
  let coords1 = S.fromList $ concatMap coordinates $ foldWire w1
      coords2 = S.fromList $ concatMap coordinates $ foldWire w2
      biglist = S.intersection coords1 coords2
   in closestIntersection biglist

solveTwo w1 w2 =
  minimum $ filter (/= 0) $ concat $ sequence $ combinedStepsToAllIntersections w1 w2

combinedStepsToAllIntersections w1 w2 =
  let intersections = S.toList $ allIntersections w1 w2
   in map (combinedStepsToIntersection w1 w2) intersections

stepsToCoordinate :: [String] -> Coordinate -> Maybe Int
stepsToCoordinate wire coord =
  let lines = foldWire wire
   in elemIndex coord $ reverse $ concatMap coordinates lines

combinedStepsToIntersection w1 w2 coordinate =
  sum
    <$> sequence
      [ stepsToCoordinate w1 coordinate,
        stepsToCoordinate w2 coordinate
      ]

allIntersectionsList w1 w2 =
  let coords1 :: [Coordinate] = concatMap coordinates $ foldWire w1
      coords2 :: [Coordinate] = concatMap coordinates $ foldWire w2
   in intersect coords1 coords2

allIntersections :: [String] -> [String] -> S.Set Coordinate
allIntersections w1 w2 =
  let coords1 :: S.Set Coordinate = S.fromList $ concatMap coordinates $ foldWire w1
      coords2 :: S.Set Coordinate = S.fromList $ concatMap coordinates $ foldWire w2
   in S.intersection coords1 coords2

closestIntersection list =
  S.findMin $ S.drop 1 $ S.map (\it -> distanceFromPort (x it) (y it)) list

closestIntersectionCoord list =
  S.map (\it -> (distanceFromPort (x it) (y it), it)) list

exInput1 = ["R8", "U5", "L5", "D3"]

exInput2 = ["U7", "R6", "D4", "L3"]

-- good test where second intersection is closer
secondCloser = allIntersections ["U1", "R1", "D1", "D2"] ["D2", "R2", "U1", "L2"]

center = Coordinate {x = 0, y = 0}

data Line = Line {coordinates :: [Coordinate]}
  deriving (Show, Eq, Ord)

data Coordinate = Coordinate {x :: Int, y :: Int}
  deriving (Show, Eq, Ord)

cartDiff x1 x2 y1 y2 = abs (x1 - x2) + abs (y1 - y2)

--takes 1 segment like "R75"  an origin Coordinate, returns a Line
makeLine :: String -> Coordinate -> Line
makeLine segment origin =
  let direction :: Char = head segment
      distance :: Int = read $ drop 1 segment :: Int
      oldX = (x origin)
      oldY = (y origin)
   in case direction of
        'R' ->
          Line
            ( reverse $
                drop 1 $
                  reverse
                    [ Coordinate {x = x', y = y'}
                      | x' <- reverse [oldX .. (oldX + distance)],
                        y' <- [oldY]
                    ]
            )
        'L' ->
          Line
            ( reverse $
                drop 1 $
                  reverse
                    [ Coordinate {x = x', y = y'}
                      | x' <- [(oldX - distance) .. oldX],
                        y' <- [oldY]
                    ]
            )
        'U' ->
          Line
            ( reverse $
                drop 1 $
                  reverse
                    [ Coordinate {x = x', y = y'}
                      | y' <- reverse [oldY .. (oldY + distance)],
                        x' <- [oldX]
                    ]
            )
        'D' ->
          Line
            ( reverse $
                drop 1 $
                  reverse
                    [ Coordinate {x = x', y = y'}
                      | y' <- [(oldY - distance) .. oldY],
                        x' <- [oldX]
                    ]
            )

distanceFromPort x y =
  cartDiff 0 x 0 y
  where
    cartDiff x1 x2 y1 y2 = abs (x1 - x2) + abs (y1 - y2)

foldWire :: [String] -> [Line]
foldWire wireInput =
  foldl'
    ( \acc seg ->
        let lastLine = head acc
            lastCoord = (head $ coordinates lastLine)
            newLine :: Line = makeLine seg lastCoord
         in newLine : (acc :: [Line])
    )
    [Line {coordinates = [center]}]
    wireInput

-- (example) input below -------------------------------------------------------

exo1a = ["R8", "U5", "L5", "D3"]

exo1b = ["U7", "R6", "D4", "L4"]

exy1a = ["R75", "D30", "R83", "U83", "L12", "D49", "R71", "U7", "L72"]

exy1b = ["U62", "R66", "U55", "R34", "D71", "R55", "D58", "R83"]

exy2a = ["R98", "U47", "R26", "D63", "R33", "U87", "L62", "D20", "R33", "U53", "R51"]

exy2b = ["U98", "R91", "D20", "R16", "D67", "R40", "U7", "R15", "U6", "R7"]

wire1 = ["R1000", "D940", "L143", "D182", "L877", "D709", "L253", "U248", "L301", "U434", "R841", "U715", "R701", "U92", "R284", "U115", "R223", "U702", "R969", "U184", "L992", "U47", "L183", "U474", "L437", "D769", "L71", "U96", "R14", "U503", "R144", "U432", "R948", "U96", "L118", "D696", "R684", "U539", "L47", "D851", "L943", "U606", "L109", "D884", "R157", "U946", "R75", "U702", "L414", "U347", "R98", "D517", "L963", "D177", "R467", "D142", "L845", "U427", "R357", "D528", "L836", "D222", "L328", "U504", "R237", "U99", "L192", "D147", "L544", "D466", "R765", "U845", "L267", "D217", "L138", "U182", "R226", "U466", "R785", "U989", "R55", "D822", "L101", "U292", "R78", "U962", "R918", "U218", "L619", "D324", "L467", "U885", "L658", "U890", "L764", "D747", "R369", "D930", "L264", "D916", "L696", "U698", "R143", "U537", "L922", "U131", "R141", "D97", "L76", "D883", "R75", "D657", "R859", "U503", "R399", "U33", "L510", "D318", "L455", "U128", "R146", "D645", "L147", "D651", "L388", "D338", "L998", "U321", "L982", "U150", "R123", "U834", "R913", "D200", "L455", "D479", "L38", "U860", "L471", "U945", "L946", "D365", "L377", "U816", "R988", "D597", "R181", "D253", "R744", "U472", "L345", "U495", "L187", "D443", "R924", "D536", "R847", "U430", "L145", "D827", "L152", "D831", "L886", "D597", "R699", "D751", "R638", "D580", "L488", "D566", "L717", "D220", "L965", "D587", "L638", "D880", "L475", "D165", "L899", "U388", "R326", "D568", "R940", "U550", "R788", "D76", "L189", "D641", "R629", "D383", "L272", "D840", "L441", "D709", "L424", "U158", "L831", "D576", "R96", "D401", "R425", "U525", "L378", "D907", "L645", "U609", "L336", "D232", "L259", "D280", "L523", "U938", "R190", "D9", "L284", "U941", "L254", "D657", "R572", "U443", "L850", "U508", "L742", "D661", "L977", "U910", "L190", "U626", "R140", "U762", "L673", "U741", "R317", "D518", "R111", "U28", "R598", "D403", "R465", "D684", "R79", "U725", "L556", "U302", "L367", "U306", "R632", "D550", "R89", "D292", "R561", "D84", "L923", "D109", "L865", "D880", "L387", "D24", "R99", "U934", "L41", "U29", "L225", "D12", "L818", "U696", "R652", "U327", "L69", "D773", "L618", "U803", "L433", "D467", "R840", "D281", "R161", "D400", "R266", "D67", "L205", "D94", "R551", "U332", "R938", "D759", "L437", "D515", "L480", "U774", "L373", "U478", "R963", "D863", "L735", "U138", "L580", "U72", "L770", "U968", "L594"]

wire2 = ["L990", "D248", "L833", "U137", "L556", "U943", "R599", "U481", "R963", "U812", "L825", "U421", "R998", "D847", "R377", "D19", "R588", "D657", "R197", "D354", "L548", "U849", "R30", "D209", "L745", "U594", "L168", "U5", "L357", "D135", "R94", "D686", "R965", "U838", "R192", "U428", "L861", "U354", "R653", "U543", "L633", "D508", "R655", "U575", "R709", "D53", "L801", "D709", "L92", "U289", "L466", "D875", "R75", "D448", "R576", "D972", "L77", "U4", "L267", "D727", "L3", "D687", "R743", "D830", "L803", "D537", "L180", "U644", "L204", "U407", "R866", "U886", "R560", "D848", "R507", "U470", "R38", "D652", "R806", "D283", "L836", "D629", "R347", "D679", "R609", "D224", "L131", "D616", "L687", "U181", "R539", "D829", "L598", "D55", "L806", "U208", "R886", "U794", "L268", "D365", "L145", "U690", "R50", "D698", "L140", "D512", "L551", "U845", "R351", "U724", "R405", "D245", "L324", "U181", "L824", "U351", "R223", "D360", "L687", "D640", "L653", "U158", "R786", "D962", "R931", "D151", "R939", "D34", "R610", "U684", "L694", "D283", "R402", "D253", "R388", "D195", "R732", "U809", "R246", "D571", "L820", "U742", "L507", "U967", "L886", "D693", "L273", "U558", "L914", "D122", "R146", "U788", "R83", "U149", "R241", "U616", "R326", "U40", "L192", "D845", "L577", "U803", "L668", "D443", "R705", "D793", "R443", "D883", "L715", "U757", "R767", "D360", "L289", "D756", "R696", "D236", "L525", "U872", "L332", "U203", "L152", "D234", "R559", "U191", "R340", "U926", "L746", "D128", "R867", "D562", "L100", "U445", "L489", "D814", "R921", "D286", "L378", "D956", "L36", "D998", "R158", "D611", "L493", "U542", "R932", "U957", "R55", "D608", "R790", "D388", "R414", "U670", "R845", "D394", "L572", "D612", "R842", "U792", "R959", "U7", "L285", "U769", "L410", "D940", "L319", "D182", "R42", "D774", "R758", "D457", "R10", "U82", "L861", "D901", "L310", "D217", "R644", "U305", "R92", "U339", "R252", "U460", "R609", "D486", "R553", "D798", "R809", "U552", "L183", "D238", "R138", "D147", "L343", "D597", "L670", "U237", "L878", "U872", "R789", "U268", "L97", "D313", "R22", "U343", "R907", "D646", "L36", "D516", "L808", "U622", "L927", "D982", "L810", "D149", "R390", "U101", "L565", "U488", "L588", "U426", "L386", "U305", "R503", "U227", "R969", "U201", "L698", "D850", "R800", "D961", "R387", "U632", "R543", "D541", "R750", "D174", "R543", "D237", "R487", "D932", "R220"]
