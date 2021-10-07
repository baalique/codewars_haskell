-- https://www.codewars.com/kata/591392af88a4994caa0000e0

module Subsets where

f :: Int -> Int
f = pred . fib . (2 +)

fib :: Int -> Int
fib = (fibs !!) where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
