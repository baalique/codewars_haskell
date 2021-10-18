-- https://www.codewars.com/kata/529e2e1f16cb0fcccb000a6b

module Split where

splitInteger :: Int -> Int -> [Int]
splitInteger total parts = replicate odds (d - 1) ++ replicate (parts - odds) d
  where
    d    = total `div` parts + 1
    odds = d * parts - total
