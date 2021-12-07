-- https://www.codewars.com/kata/59e66e48fc3c499ec5000103

module ArrayCombos where

import           Data.List                      ( nub
                                                , sort
                                                )
                                                
solve :: [[Int]] -> Int
solve = product . map (length . nub . sort)
