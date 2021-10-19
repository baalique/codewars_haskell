-- https://www.codewars.com/kata/59bb02f5623654a0dc000119

module BabyMagpies where

child :: String -> String -> Bool
child = diffInRange [1 .. 2]

grandchild :: String -> String -> Bool
grandchild [h1] [h2] = h1 == h2
grandchild m1   m2   = diffInRange [0 .. 4] m1 m2

diffInRange :: [Int] -> String -> String -> Bool
diffInRange range x1 x2 = (`elem` range) $ length $ filter (uncurry (/=)) $ zip x1 x2
