-- https://www.codewars.com/kata/5b3e609cd58499284100007a

module ArrayProductSansN.Kata where

productSansN :: [Integer] -> [Integer]
productSansN xs = map conv xs
 where
  prod = product $ filter (/= 0) xs
  conv = case length $ filter (== 0) xs of
    0 -> (prod `div`)
    1 -> \x -> if x == 0 then prod else 0
    _ -> const 0
