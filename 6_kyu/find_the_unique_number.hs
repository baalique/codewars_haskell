-- https://www.codewars.com/kata/55f81f9aa51f9b72a200002f

module Codewars.Kata.Unique where

import           Data.Function                  ( on )
import           Data.List                      ( group
                                                , minimumBy
                                                , sort
                                                )

findUnique :: [Int] -> Int
findUnique = head . minimumBy (compare `on` length) . group . sort
