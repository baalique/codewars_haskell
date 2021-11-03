-- https://www.codewars.com/kata/526d84b98f428f14a60008da

module Hamming where

import           Data.List                      ( nub
                                                , sort
                                                )

hamming :: Int -> Int
hamming n = (!! (n - 1)) $ genHamming n [1]

genHamming :: Int -> [Int] -> [Int]
genHamming n xs | length xs > 3 * n = take n xs
                | otherwise         = genHamming n (nub $ sort $ xs >>= nextHamming)

nextHamming :: Int -> [Int]
nextHamming n = map (* n) [1, 2, 3, 5]
