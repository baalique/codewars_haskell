-- https://www.codewars.com/kata/581e014b55f2c52bb00000f8

module Kata where

import           Data.Char                      ( chr
                                                , isNumber
                                                )

decipherThis :: String -> String
decipherThis = unwords . map (replaceSecondAndLast . changeFirst) . words

changeFirst :: [Char] -> [Char]
changeFirst xs = (chr . read . takeWhile isNumber) xs : dropWhile isNumber xs

replaceSecondAndLast :: [Char] -> [Char]
replaceSecondAndLast (x : y : xs@(_ : _)) = x : last xs : init xs ++ [y]
replaceSecondAndLast xs                   = xs
