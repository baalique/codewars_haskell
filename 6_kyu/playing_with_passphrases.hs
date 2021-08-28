-- https://www.codewars.com/kata/559536379512a64472000053

module Codewars.Kata.PlayPass where

import           Data.Char                      ( digitToInt
                                                , intToDigit
                                                , toLower
                                                , toUpper
                                                )
import           Data.List                      ( elemIndex )

playPass :: String -> Int -> String
playPass xs delta = reverse . switchCase . replaceDigits . shiftLetters delta $ xs

shiftLetters :: Int -> String -> String
shiftLetters delta = map (\c -> if c `elem` abc then moveLetter c delta else c)
  where
    moveLetter chr delta = (!!) abc (mod ((\(Just x) -> x) (elemIndex chr abc) + delta + length abc) (length abc))

abc :: [Char]
abc = ['A' .. 'Z']

digits :: [Char]
digits = ['0' .. '9']

replaceDigits :: String -> String
replaceDigits = map (\c -> if c `elem` digits then intToDigit . (9 -) . digitToInt $ c else c)

switchCase :: String -> String
switchCase = zipWith (\f c -> f c) (cycle [toUpper, toLower])
