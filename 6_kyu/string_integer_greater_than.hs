-- https://www.codewars.com/kata/54d3eae3525c153b21000e3b

module Codewars.Kata.SIGT where

import           Prelude                 hiding ( Integer
                                                , fromInteger
                                                , fromIntegral
                                                , read
                                                , reads
                                                , readsPrec
                                                , toInteger
                                                )

stringIntGreaterThan :: String -> String -> Bool
stringIntGreaterThan xs'@(x : xs) ys'@(y : ys) | xs' == ys'              = False
                                               | null xs' && null ys'    = False
                                               | x == '-' && y /= '-'    = False
                                               | x /= '-' && y == '-'    = True
                                               | x == '-' && y == '-'    = not $ stringIntGreaterThan xs ys
                                               | length xs' > length ys' = True
                                               | length xs' < length ys' = False
                                               | null xs'                = False
                                               | null ys'                = True
                                               | otherwise = if x == y then stringIntGreaterThan xs ys else x > y
stringIntGreaterThan _ _ = undefined
