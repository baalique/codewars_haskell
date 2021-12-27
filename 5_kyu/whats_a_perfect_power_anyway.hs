-- https://www.codewars.com/kata/54d4c8b08776e4ad92000835

module Codewars.Kata.PP where

isPP :: Integer -> Maybe (Integer, Integer)
isPP n = checkPerfect [2 .. floor $ sqrt (fromIntegral n)] n

checkPerfect :: [Integer] -> Integer -> Maybe (Integer, Integer)
checkPerfect []       n = Nothing
checkPerfect (x : xs) n = if x ^ p == n then Just (x, p) else checkPerfect xs n
    where p = round $ logBase (fromIntegral x) (fromIntegral n)
