-- https://www.codewars.com/kata/5a296b571f7f70b0b3000100

module ArraysListsSets where

import           Data.Function                  ( on )
import           Data.List                      ( groupBy
                                                , nub
                                                , sort
                                                , sortBy
                                                )

solve :: [[Char]] -> [Int]
solve =
    sort
        . map (sum . map fst)
        . filter ((> 1) . length)
        . groupBy (\x y -> snd x == snd y)
        . sortBy (compare `on` snd)
        . zip [0 ..]
        . map (sort . nub)
