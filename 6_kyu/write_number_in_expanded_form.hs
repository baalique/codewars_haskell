-- https://www.codewars.com/kata/5842df8ccbd22792a4000245

module Kata where

import           Data.List

expandedForm :: Int -> String
expandedForm n = intercalate " + " decimals
 where
  decimals  = map (\p -> fst p : replicate (snd p) '0') withIndex
  withIndex = filter (\p -> fst p /= '0') (zip str (reverse [0 .. length str - 1]))
  str       = show n
