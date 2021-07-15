-- https://www.codewars.com/kata/5208f99aee097e6552000148

module Codewars.Kata.BreakCamelCase where

import           Data.Char

solution :: String -> String
solution str = splitCamelCase str ""

splitCamelCase :: String -> String -> String
splitCamelCase [] acc                   = trimLeadingSpace $ reverse acc
splitCamelCase (x : xs) acc | isUpper x = splitCamelCase xs (x : ' ' : acc)
splitCamelCase (x : xs) acc             = splitCamelCase xs (x : acc)

trimLeadingSpace :: String -> String
trimLeadingSpace (' ' : xs) = xs
trimLeadingSpace xs         = xs
