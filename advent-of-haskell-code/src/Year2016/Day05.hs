{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day05
  (
  )
where

import Data.Digest.Pure.MD5
import qualified Data.List.Split as Split
import Data.Text.Lazy.Encoding as Enc
import RIO
import RIO.Char (isDigit)
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text.Lazy as TL
import Prelude
  ( head,
    read,
    (!!),
  )

{--
Felt like playing with RIO Text so a lot of clunky string conversion going on.
--}

-- get rid of hardcoded ranges by making it look UNTIL it finds 8 valid characters
answerOne = getAnswer input [702868 .. 12000000]

-- takes long to run but eventually completes
answerTwo = getAnswer input [702868 .. 30000000]

getAnswer :: TL.Text -> [Int] -> String
getAnswer inp nums =
  L.map
    ( \n ->
        let hash' = concatHash inp n
         in (isFirstFiveZero hash', TL.unpack hash' !! 5, TL.unpack hash' !! 6)
    )
    nums
    & L.filter ((== True) . (\(a, b, c) -> a))
    & L.filter (\(a, b, c) -> isValidPosition b)
    & L.map (\(a, b, c) -> (b, c))
    & L.nubBy (\x y -> fst x == fst y) -- nubBy returns unique elements, retaining first occurence "f12f9cc73009de6f75" -> "f2c730e5"
    & L.sortBy (compare `on` fst)
    & L.map snd

isValidPosition :: Char -> Bool
isValidPosition c = isDigit c && (read [c] < 8)

isFirstFiveZero :: TL.Text -> Bool
isFirstFiveZero t = TL.all (== '0') (TL.take 5 t)

-- Concats a number to a string of text and produces an md5 hash
concatHash :: TL.Text -> Int -> TL.Text
concatHash t n = TL.pack . show . md5 . Enc.encodeUtf8 $ t <> TL.pack (show n)

myHash = md5 $ Enc.encodeUtf8 input

input = TL.pack "ugkcyxxp"

exampleInput = TL.pack "abc"
