-- https://www.codewars.com/kata/584f6da5ddf34867fc000048

module DistributeGifts.Kata where

import           Data.Function                  ( on )
import           Data.List                      ( sortBy )

distributeGifts :: [(Int, Int)] -> Int
distributeGifts xs = sum $ map snd minHalf ++ map fst maxHalf
    where (minHalf, maxHalf) = splitAt (length xs `div` 2) (sortBy (compare `on` uncurry subtract) xs)
