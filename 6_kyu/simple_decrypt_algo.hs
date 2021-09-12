-- https://www.codewars.com/kata/58693136b98de0e4910001ab

module Kata where

import           Data.Char                      ( intToDigit )

decrypt :: [Char] -> [Char]
decrypt key = map (intToDigit . \c -> length $ filter (== c) key) ['a' .. 'z']
