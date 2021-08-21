-- https://www.codewars.com/kata/5245a9138ca049e9a10007b8

module AdjacentPairs where

import           Data.Char                      ( toLower )

countAdjacentPairs :: String -> Int
countAdjacentPairs = getAdjacent . words . map toLower

getAdjacent :: [String] -> Int
getAdjacent = g . foldr f ("", 0, 0)
 where
  f = \x (prev, c, acc) -> if x == prev then (x, c + 1, acc) else (x, 0, acc + h c)
  g = \(_, c, acc) -> acc + h c
  h = \c -> if c > 0 then 1 else 0
  