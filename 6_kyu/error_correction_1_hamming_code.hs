-- https://www.codewars.com/kata/5ef9ca8b76be6d001d5e1c3e

module Hamming where

import           Data.Char                      ( chr
                                                , digitToInt
                                                , intToDigit
                                                , ord
                                                )
import           Data.List                      ( elemIndices
                                                , group
                                                , maximumBy
                                                , sort
                                                )
import           Data.List.Split                ( chunksOf )

encode :: [Char] -> [Char]
encode = map intToDigit . concatMap (concatMap (replicate 3) . completeWithZeros . decToBin . ord)

decode :: [Char] -> [Char]
decode = map (chr . binToDec . map digitToInt) . chunksOf 8 . map mostFrequent . chunksOf 3

decToBin :: Int -> [Int]
decToBin 0 = [0]
decToBin x = reverse $ toBin' x
 where
  toBin' 0 = []
  toBin' x | x `mod` 2 == 1 = 1 : toBin' (x `div` 2)
           | otherwise      = 0 : toBin' (x `div` 2)

binToDec :: [Int] -> Int
binToDec = sum . map (2 ^) . elemIndices 1 . reverse

completeWithZeros :: [Int] -> [Int]
completeWithZeros xs = replicate (8 - length xs) 0 ++ xs

mostFrequent :: (Eq a, Ord a) => [a] -> a
mostFrequent = head . maximumBy (\a b -> compare (length a) (length b)) . group . sort
