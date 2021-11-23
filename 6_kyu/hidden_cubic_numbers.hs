-- https://www.codewars.com/kata/55031bba8cba40ada90011c4

module Codewars.Kata.CubicNumbers where

import           Codewars.Kata.CubicNumbers.Types
import           Data.Char                      ( digitToInt
                                                , isNumber
                                                )
import           Data.List.Split                ( chunksOf )

sumOfCubes :: String -> Maybe Lucky
sumOfCubes str =
    let cubicNumbers = filter isCubic $ getNumbers str
        numbers      = map read cubicNumbers
    in  case length numbers of
            0 -> Nothing
            _ -> Just $ Lucky numbers (sum numbers)

isCubic :: [Char] -> Bool
isCubic xs = length xs <= 3 && sum (map ((^ 3) . digitToInt) xs) == read xs

getNumbers :: [Char] -> [[Char]]
getNumbers xs = splitBy isNumber xs >>= splitToGroups

splitToGroups :: [e] -> [[e]]
splitToGroups = chunksOf 3

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f xs = if null hd then splitBy f tl else hd : splitBy f tl
  where
    (hd, tl') = span f xs
    (_ , tl ) = break f tl'
