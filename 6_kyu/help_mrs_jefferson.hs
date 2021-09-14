-- https://www.codewars.com/kata/59321f29a010d5aa80000066

module Jefferson where

import           Data.Function                  ( on )
import           Data.List                      ( minimumBy )

shortestArrang :: Int -> [Int]
shortestArrang = getArrang . getAllProgressions
  where
    getArrang [] = [-1]
    getArrang xs = reverse . genProgression . minimumBy (compare `on` fst) $ xs
    genProgression (n, a1) = [a1 .. a1 + n - 1]
    getAllProgressions x = [ (n, a1) | a1 <- [1 .. x], n <- [2 .. x], (2 * a1 + n - 1) * n == 2 * x ]
