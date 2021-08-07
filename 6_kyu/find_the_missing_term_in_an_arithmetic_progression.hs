-- https://www.codewars.com/kata/52de553ebb55d1fca3000371

module Codewars.Kata.Arithmetic where

findMissing :: Integral n => [n] -> n
findMissing xs = findMissing' (getStep xs) xs
 where
  findMissing' step (x : y : xs) | x + step /= y = x + step
  findMissing' step xs                           = findMissing' step (tail xs)

getStep :: (Ord p, Num p) => [p] -> p
getStep (a : b : c : _) = min (c - b) (b - a)
getStep _               = undefined
