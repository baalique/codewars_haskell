-- https://www.codewars.com/kata/60a516d70759d1000d532029

module Survivors where

survivors :: [Int] -> [[Int]] -> [Int]
survivors vals xs = map fst $ filter (\(_, ys) -> all (> 0) ys) $ zip [0 ..] $ zipWith goThrough vals xs

goThrough :: Int -> [Int] -> [Int]
goThrough val xs = reverse $ foldr (\x acc -> (head acc - 1 + x) : acc) [val] (reverse xs)
