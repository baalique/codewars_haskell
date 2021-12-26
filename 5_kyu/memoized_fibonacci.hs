-- https://www.codewars.com/kata/529adbf7533b761c560004e5

module Fibonacci where

fibonacci :: Int -> Integer
fibonacci = (fibs !!) where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
