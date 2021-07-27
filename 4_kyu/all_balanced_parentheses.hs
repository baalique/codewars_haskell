-- https://www.codewars.com/kata/5426d7a2c2c7784365000783

module Balanced.Parens where

balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens n = [ "(" ++ l ++ ")" ++ r | m <- [0 .. n - 1], l <- balancedParens m, r <- balancedParens (n - m - 1) ]
