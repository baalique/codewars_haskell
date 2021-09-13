-- https://www.codewars.com/kata/5fc7d2d2682ff3000e1a3fbc

module MessageValidator where

import           Data.Char                      ( isDigit
                                                , isLetter
                                                )
import           Data.List.Split                ( splitWhen )
import           Text.Regex.TDFA                ( (=~) )

isAValidMessage :: [Char] -> Bool
isAValidMessage msg = (msg =~ regex) && all (\(d, l) -> d == length l) (zip digits letters)
  where
    digits  = map (\x -> read x :: Int) . filter (/= "") . splitWhen isLetter $ msg
    letters = filter (/= "") . splitWhen isDigit $ msg
    regex   = "^([0-9]+[a-zA-Z]+)*$"
