-- https://www.codewars.com/kata/5894134c8afa3618c9000146

module Chess where

import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )

sameColor :: String -> String -> Bool
sameColor [l1, n1] [l2, n2] = diffMod l1 l2 letters == diffMod n1 n2 numbers
sameColor _ _ = undefined

letters :: [Char]
letters = ['A' .. 'H']

numbers :: [Char]
numbers = ['1' .. '8']

distance :: Eq a => a -> a -> [a] -> Int
distance c1 c2 abc = abs $ getIndex c1 - getIndex c2 where getIndex c = fromJust $ elemIndex c abc

diffMod :: Eq a => a -> a -> [a] -> Bool
diffMod c1 c2 abc = (== 0) $ (`mod` 2) $ distance c1 c2 abc
