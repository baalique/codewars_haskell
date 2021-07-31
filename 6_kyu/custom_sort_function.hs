-- https://www.codewars.com/kata/52105fab0bd0ce9dd00000fe

module Codewars.Kata.Sort where

sort :: Ord a => [a] -> [a]
sort []       = []
sort (x : xs) = sort left ++ middle ++ sort right
  where
    left   = [ y | y <- xs, y < x ]
    middle = [ y | y <- xs, y == x ] ++ [x]
    right  = [ y | y <- xs, y > x ]
