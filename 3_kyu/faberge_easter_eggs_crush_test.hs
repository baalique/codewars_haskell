-- https://www.codewars.com/kata/54cb771c9b30e8b5250011d4

module Faberge where

heigth :: Integer -> Integer -> Integer
heigth n m = sum $ getBinomials m n

getBinomials :: Integer -> Integer -> [Integer]
getBinomials n 0 = [0]
getBinomials n k = getBinomials' n k 1 [n]
  where
    getBinomials' n k cur acc | cur == k = acc
    getBinomials' n k cur acc            = getBinomials' n k (cur + 1) (head acc * (n - cur) `div` (cur + 1) : acc)
