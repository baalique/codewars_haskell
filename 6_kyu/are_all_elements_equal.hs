-- https://www.codewars.com/kata/5963caa2cb97be79630000c0

module EqAll.Kata where

eqAll :: (Eq a) => [a] -> Bool
eqAll = \xs -> all (== head xs) xs
