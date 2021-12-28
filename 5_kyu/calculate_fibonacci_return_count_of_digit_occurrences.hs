-- https://www.codewars.com/kata/5779f894ec8832493f00002d

module Kata where

import           Data.Char                      ( digitToInt )
import           Data.List                      ( group
                                                , sort
                                                , sortOn
                                                )
import           Data.Ord                       ( Down(Down) )

fibDigits :: Integer -> [(Integer, Integer)]
fibDigits =
    sortOn Down
        . map (\xs -> (toInteger $ length xs, toInteger $ digitToInt $ head xs))
        . group
        . sort
        . show
        . fib

fib :: Integer -> Integer
fib = (fibs !!) . fromIntegral where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
