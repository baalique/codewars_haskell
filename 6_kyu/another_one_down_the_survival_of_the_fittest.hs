-- https://www.codewars.com/kata/563ce9b8b91d25a5750000b6

module Codewars.Kata.RemoveSmallest where

import           Data.List                      ( elemIndex
                                                , sort
                                                )
import           Data.Maybe                     ( fromJust )

removeSmallest :: Int -> [Int] -> [Int]
removeSmallest n xs = foldr deleteFromList xs smallest where smallest = take n $ sort xs

deleteFromList :: Eq a => a -> [a] -> [a]
deleteFromList val xs = take idx xs ++ drop (idx + 1) xs where idx = fromJust $ elemIndex val xs
