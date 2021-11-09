-- https://www.codewars.com/kata/557e8a141ca1f4caa70000a6

module Codewars.Kata.TriangleNumbers where

isTriangleNumber :: Integer -> Bool
isTriangleNumber n = isSquare (8 * n + 1)

isSquare :: Integral a => a -> Bool
isSquare n = sq * sq == n where sq = floor $ sqrt (fromIntegral n :: Double)
