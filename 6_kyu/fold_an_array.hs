-- https://www.codewars.com/kata/57ea70aa5500adfe8a000110

module Folded.MyLists where

foldList :: [Int] -> Int -> [Int]
foldList xs n = foldl (\acc _ -> foldHalves acc) xs [0 .. n - 1]

foldHalves :: Num a => [a] -> [a]
foldHalves xs = zipWith (+) firstHalf secondHalf ++ [ xs !! halfLength | odd (length xs) ]
  where
    firstHalf  = take halfLength xs
    secondHalf = reverse $ drop halfLength xs
    halfLength = length xs `div` 2
