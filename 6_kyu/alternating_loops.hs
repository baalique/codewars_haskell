-- https://www.codewars.com/kata/55e529bf6c6497394a0000b5

module Codewars.Kata.Alternate where

combine :: [[a]] -> [a]
combine [] = []
combine xs = concat [ map (!! idx) $ filter (\ys -> length ys > idx) xs | idx <- [0 .. maxLen - 1] ]
    where maxLen = maximum $ map length xs
