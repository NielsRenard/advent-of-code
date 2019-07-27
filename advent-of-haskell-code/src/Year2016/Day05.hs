{-# LANGUAGE NoImplicitPrelude #-}

module Year2016.Day05
  ()
where

import           Prelude                        ( read
                                                , head
                                                , (!!)
                                                )
import qualified Data.List.Split               as Split
import           RIO
import qualified RIO.Map                       as M
import           RIO.Char                       ( isDigit )
import qualified RIO.List                      as L
import qualified RIO.Text.Lazy                 as TL
import           Data.Text.Lazy.Encoding as Enc
import Data.Digest.Pure.MD5

{--
Felt like playing with RIO Text so a lot of clunky string conversion going on.
--}

{--
answerOne input [1..10000000]
[(True,702868),(True,1776010),(True,8421983),(True,8744114),(True,8845282),(True,9268910),(True,9973527)]
--}

-- answer takes a good twenty seconds now
-- something something with isFirstFiveZero, immediately False out if a char isn't 0
answerOne = getAnswer input [702868..12000000]

getAnswer :: TL.Text -> [Int] -> String
getAnswer inp nums =
  L.map (\n -> let hash' = concatHash inp n
               in (isFirstFiveZero hash', TL.unpack hash' !! 5)) nums
  & L.filter ((== True) . fst)
  & map snd

isFirstFiveZero :: TL.Text -> Bool
isFirstFiveZero t = TL.take 5 t == TL.pack ([1..5] *> "0")

-- Concats a number to a string of text and produces an md5 hash
concatHash :: TL.Text -> Int -> TL.Text
concatHash t n = TL.pack . show . md5 . Enc.encodeUtf8 $ t <> TL.pack (show n)

myHash = md5 $ Enc.encodeUtf8 input
input = TL.pack "ugkcyxxp"
exampleInput = TL.pack "abc"
