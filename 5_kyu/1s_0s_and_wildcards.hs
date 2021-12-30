-- https://www.codewars.com/kata/588f3e0dfa74475a2600002a

module Kata where

possibilities :: String -> [String]
possibilities []         = []
possibilities "0"        = ["0"]
possibilities "1"        = ["1"]
possibilities "?"        = ["0", "1"]
possibilities ('?' : xs) = concatMap (\x -> [ x : ys | ys <- possibilities xs ]) "01"
possibilities (x   : xs) = [ x : ys | ys <- possibilities xs ]
