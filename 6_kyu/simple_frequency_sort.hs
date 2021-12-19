-- https://www.codewars.com/kata/5a8d2bf60025e9163c0000bc

module FreqSort where

import           Data.Function                  ( on )
import           Data.List                      ( group
                                                , sort
                                                , sortBy
                                                )

solve :: [Int] -> [Int]
solve = concat . sortBy (compare `on` (negate . length)) . group . sort
