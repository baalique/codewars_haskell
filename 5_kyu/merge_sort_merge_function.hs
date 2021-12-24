-- https://www.codewars.com/kata/52336a4436e0b095d8000093

module Merge where

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x : xs') ys@(y : ys') | x <= y    = x : merge xs' ys
                                | otherwise = y : merge xs ys'
