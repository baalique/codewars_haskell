-- https://www.codewars.com/kata/5616868c81a0f281e500005c

module Codewars.G964.Rank where

import           Data.Char                      ( toUpper )
import           Data.List                      ( elemIndex
                                                , sortBy
                                                )
import           Data.List.Split                ( splitOn )

rank :: String -> [Int] -> Int -> String
rank "" _ _          = "No participants"
rank names weights r | r > length (splitOn "," names) = "Not enough participants"
rank names weights r = fst $ (!! (r - 1)) $ sortBy sorter $ zipWith (\n w -> (n, getN n w)) ns weights
  where
    sorter = \(n1, w1) (n2, w2) -> compare (-w1, n1) (-w2, n2)
    ns     = splitOn "," names

abc :: String
abc = ['A' .. 'Z']

getN :: String -> Int -> Int
getN n w = (w *) $ (2 * length n) + sum (map (((\(Just x) -> x) . (`elemIndex` abc)) . toUpper) n)
