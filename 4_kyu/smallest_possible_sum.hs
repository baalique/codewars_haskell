-- https://www.codewars.com/kata/52f677797c461daaf7000740

module Kata.SmallestPossibleSum where

smallestPossibleSum :: (Integral a) => [a] -> a
smallestPossibleSum xs = foldl1 gcd xs * fromIntegral (length xs)
