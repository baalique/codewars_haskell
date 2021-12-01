-- https://www.codewars.com/kata/5bb5880f3cc0597aae00003f

module MinFree where

import           Data.List                      ( sort )

minfree :: [Int] -> Int
minfree = getMin 0 . sort

getMin :: Int -> [Int] -> Int
getMin n []              = n
getMin n (x : _) | n < x = n
getMin n (_ : xs)        = getMin (n + 1) xs
