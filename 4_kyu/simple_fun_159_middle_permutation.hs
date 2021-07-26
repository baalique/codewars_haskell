-- https://www.codewars.com/kata/58ad317d1541651a740000c5

module MiddlePermutation.JorgeVS.Kata where

import           Data.List                      ( sort )

middlePermutation :: String -> String
middlePermutation str = (nThPermutation middleNumber . sort) str where middleNumber = fact (length str) `div` 2 - 1

nThPermutation :: Integer -> [a] -> [a]
nThPermutation n abc = getNThPermutation n abc ((rem n . fact . (\x -> x - 1) . length) abc) []

getNThPermutation :: Integer -> [a] -> Integer -> [a] -> [a]
getNThPermutation n abc r acc = case r of
    0 -> reverse acc' ++ abc'
    _ -> getNThPermutation r abc' (rem n (fact (length abc' - 1))) acc'
  where
    idx  = fromIntegral $ n `div` fact (length abc - 1)
    acc' = abc !! idx : acc
    abc' = take idx abc ++ drop (idx + 1) abc

fact :: Int -> Integer
fact n = product [1 .. fromIntegral n]
