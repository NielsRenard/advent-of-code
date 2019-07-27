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

-- get rid of hardcoded ranges by making it look UNTIL it finds 8 valid characters
answerOne = getAnswer input [702868..12000000]
-- takes long to run but eventually completes
answerTwo = getAnswer input [702868..30000000]

-- TODO: Only use the first hit for any position
-- "f12f9cc73009de6f75" -> "f2c730e5"
getAnswer :: TL.Text -> [Int] -> String
getAnswer inp nums =
  L.map (\n -> let hash' = concatHash inp n
               in (isFirstFiveZero hash', TL.unpack hash' !! 5, TL.unpack hash' !! 6)) nums
  & L.filter ((== True) . (\(a,b,c) -> a))
  & L.filter (\(a,b,c) -> isValidPosition b)
  & L.sortBy (compare `on` (\(a,b,c)->b))
  & L.map (\(a,b,c) -> c)


isValidPosition :: Char -> Bool
isValidPosition c = isDigit c && (read [c] < 8)


isFirstFiveZero :: TL.Text -> Bool
isFirstFiveZero t = TL.take 5 t == TL.pack ([1..5] *> "0")

-- Concats a number to a string of text and produces an md5 hash
concatHash :: TL.Text -> Int -> TL.Text
concatHash t n = TL.pack . show . md5 . Enc.encodeUtf8 $ t <> TL.pack (show n)

myHash = md5 $ Enc.encodeUtf8 input
input = TL.pack "ugkcyxxp"
exampleInput = TL.pack "abc"
