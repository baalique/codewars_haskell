-- https://www.codewars.com/kata/54ba84be607a92aa900000f1

module Isogram where

import           Data.Char

isIsogram :: String -> Bool
isIsogram str = length lowCased == length (dedup 1 lowCased)
 where
  lowCased   = map toLower str
  dedup _ xs = foldl (\acc x -> if x `elem` acc then acc else x : acc) [] xs
