-- https://www.codewars.com/kata/52b757663a95b11b3d00062d

module WeIrDStRiNgCaSe where


import           Data.Char


toWeirdCase :: String -> String
toWeirdCase str = unwords (map convertWord (words str))

convertWord :: String -> String
convertWord = zipWith (\idx -> if even idx then toUpper else toLower) [0 ..]
