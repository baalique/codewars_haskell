-- https://www.codewars.com/kata/5873b2010565844b9100026d

module Kata where

oneTwoThree :: Integer -> [Integer]
oneTwoThree 0 = [0, 0]
oneTwoThree n = [read $ p1 ++ p2, read p3]
  where
    p1 = replicate (fromIntegral d) '9'
    p2 = if n `mod` 9 == 0 then "" else show (n - d * 9)
    p3 = replicate (fromIntegral n) '1'
    d  = n `div` 9
