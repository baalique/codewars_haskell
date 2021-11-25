-- https://www.codewars.com/kata/5254bd1357d59fbbe90001ec

module Codewars.Kata.Sequences where

getScore :: Integer -> Integer
getScore n = n * (n + 1) `div` 2 * 50
