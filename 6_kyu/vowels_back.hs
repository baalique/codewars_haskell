-- https://www.codewars.com/kata/57cfd92c05c1864df2001563

module VowelBack where

import           Data.List                      ( elemIndex )

vowelBack :: String -> String
vowelBack = map transform

transform :: Char -> Char
transform chr | transform' chr `elem` "code" = chr
transform chr = transform' chr

transform' :: Char -> Char
transform' 'c' = moveLetter 'c' (-1)
transform' 'o' = moveLetter 'o' (-1)
transform' 'd' = moveLetter 'd' (-3)
transform' 'e' = moveLetter 'e' (-4)
transform' 'a' = moveLetter 'a' (-5)
transform' 'i' = moveLetter 'i' (-5)
transform' 'u' = moveLetter 'u' (-5)
transform' chr = moveLetter chr 9

moveLetter :: Char -> Int -> Char
moveLetter chr delta = (!!) abc (mod (idx + delta + length abc) (length abc))
  where
    abc = ['a' .. 'z']
    idx = (\(Just x) -> x) (elemIndex chr abc)
