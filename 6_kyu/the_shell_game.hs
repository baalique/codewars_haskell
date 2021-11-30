-- https://www.codewars.com/kata/546a3fea8a3502302a000cd2

module TheShellGame where

findTheBall :: Int -> [(Int, Int)] -> Int
findTheBall = foldl swapIndex

swapIndex :: Int -> (Int, Int) -> Int
swapIndex idx (x, y) | idx == x  = y
                     | idx == y  = x
                     | otherwise = idx
