-- https://www.codewars.com/kata/5dad6e5264e25a001918a1fc

module ReversingAProcess where

import           Data.Char                      ( isDigit )
import           Data.List                      ( elemIndex
                                                , nub
                                                )
import           Data.List.Split                ( wordsBy )
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )

decode :: String -> Either String String
decode xs = if isNothing $ conv num then imp else Right res
 where
  imp = Left "Impossible to decode"
  num = read $ takeWhile isDigit xs :: Int
  str = head $ wordsBy isDigit xs
  res = map (toAbc . fromJust (conv num) . toInt) str

conv :: Int -> Maybe (Int -> Int)
conv num = if length t == length (nub t) then Just (\x -> fromJust $ elemIndex x t) else Nothing
 where
  f = \n c -> head $ filter (== n * c `mod` 26) [0 .. 25]
  t = map (f num) [0 .. 25]

toAbc :: Int -> Char
toAbc = (abc !!)

toInt :: Char -> Int
toInt = fromJust . (`elemIndex` abc)

abc :: [Char]
abc = ['a' .. 'z']
