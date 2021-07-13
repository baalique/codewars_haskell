-- https://www.codewars.com/kata/5626b561280a42ecc50000d1

module Codewars.G964.Sumdigpow where

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = filter isSumDigPow [a .. b]

isSumDigPow :: Int -> Bool
isSumDigPow x = getSumDigPow x == x
  where
    getSumDigPow = sum . getPows . zip [1 ..] . show
    getPows      = map (\p -> (^) (read [snd p] :: Int) (fst p))
