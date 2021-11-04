-- https://www.codewars.com/kata/556e0fccc392c527f20000c5

module Xbonacci where

xbonacci :: (Eq a, Num a) => [a] -> Int -> [a]
xbonacci xs n = take n $ getXbonacci xs

getXbonacci :: (Eq a, Num a) => [a] -> [a]
getXbonacci xs = xs ++ zipWithN (+) tails where tails = [ drop i (getXbonacci xs) | i <- [0 .. length xs - 1] ]

zipWithN :: Eq a => (a -> a -> a) -> [[a]] -> [a]
zipWithN f xs | [] `elem` xs = []
              | otherwise    = foldl1 f (map head xs) : zipWithN f (map tail xs)
