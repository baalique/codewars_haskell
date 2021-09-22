-- https://www.codewars.com/kata/582aad136755daf91a000021

module IntegerSequences where

import           Data.List                      ( sortOn )

findSequences :: Int -> [[Int]]
findSequences x = sortOn length [ [a1 .. a1 + n - 1] | a1 <- [1 .. x], n <- divisors, (2 * a1 + n - 1) * n == 2 * x ]
    where divisors = filter (\d -> (2 * x) `mod` d == 0) [2 .. x]
