-- https://www.codewars.com/kata/521ef596c106a935c0000519

module PrimeTime where

import           Data.List                      ( (\\) )

prime :: Int -> [Int]
prime n = sieve [2 .. n] n

sieve :: [Int] -> Int -> [Int]
sieve []       _ = []
sieve (x : xs) n = x : sieve (xs \\ [x, x + x .. n]) n
