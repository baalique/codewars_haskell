-- https://www.codewars.com/kata/5a1cb5406975987dd9000028

module SortOddEvenDifferently where

import           Data.List                      ( sort )

paritySort :: [Int] -> [Int]
paritySort xs = merge evens odds
  where
    f g h = uncurry zip $ (\(vs, is) -> (h vs, is)) $ unzip $ filter (g . fst) (zip xs [0 ..])
    evens = f even (reverse . sort)
    odds  = f odd sort

merge :: [(Int, Int)] -> [(Int, Int)] -> [Int]
merge [] [] = []
merge [] s2 = map fst s2
merge s1 [] = map fst s1
merge s1'@((v1, i1) : s1) s2'@((v2, i2) : s2) | i1 < i2   = v1 : merge s1 s2'
                                              | otherwise = v2 : merge s1' s2
