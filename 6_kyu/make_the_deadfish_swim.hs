-- https://www.codewars.com/kata/51e0007c1f9378fa810002a9

module Kata.Deadfish where

parse :: String -> [Int]
parse xs = move xs 0 []

move :: String -> Int -> [Int] -> [Int]
move []         val acc = reverse acc
move ('i' : xs) val acc = move xs (val + 1) acc
move ('d' : xs) val acc = move xs (val - 1) acc
move ('s' : xs) val acc = move xs (val ^ 2) acc
move ('o' : xs) val acc = move xs val (val : acc)
move (x   : xs) val acc = move xs val acc
