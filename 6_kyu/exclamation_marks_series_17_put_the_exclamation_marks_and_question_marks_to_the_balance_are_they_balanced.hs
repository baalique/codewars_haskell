-- https://www.codewars.com/kata/57fb44a12b53146fe1000136

{-# LANGUAGE LambdaCase #-}

module Kata where

import           Preloaded                      ( Comparison(..) )
import           Prelude                 hiding ( Either(..) )


balance :: [Char] -> [Char] -> Comparison
balance left right = case compare (weight left) (weight right) of
    GT -> Left
    EQ -> Balance
    LT -> Right

weight :: [Char] -> Int
weight = sum . map
    (\case
        '!' -> 2
        '?' -> 3
        _   -> 0
    )
