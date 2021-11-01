-- https://www.codewars.com/kata/52f78966747862fc9a0009ae

module RPN where

calc :: String -> Double
calc = parseRPN [] . words

parseRPN :: [Double] -> [[Char]] -> Double
parseRPN []           []         = 0
parseRPN [x         ] []         = x
parseRPN (x : y : xs) ("+" : ys) = parseRPN (y + x : xs) ys
parseRPN (x : y : xs) ("-" : ys) = parseRPN (y - x : xs) ys
parseRPN (x : y : xs) ("*" : ys) = parseRPN (y * x : xs) ys
parseRPN (x : y : xs) ("/" : ys) = parseRPN (y / x : xs) ys
parseRPN xs           (y   : ys) = parseRPN ((read y :: Double) : xs) ys
parseRPN _            _          = undefined
