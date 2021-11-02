-- https://www.codewars.com/kata/5226eb40316b56c8d500030f

module Codewars.Kata.PascalsTriangle where

pascalsTriangle :: Int -> [Int]
pascalsTriangle n = concat $ scanl (\acc _ -> nextRow acc) [1] [1 .. n - 1]

nextRow :: [Int] -> [Int]
nextRow row = [head row] ++ zipWith (+) row (tail row) ++ [last row]
