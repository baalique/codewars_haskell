-- https://www.codewars.com/kata/5e4d8a53b499e20016b018a0

module ExponentialGolombDecoder where

import           Data.Char
import           Data.List                      ( elemIndices
                                                , unfoldr
                                                )

decoder :: String -> [Int]
decoder = map (subtract 1 . binToDec . map digitToInt) . splitBits

splitBits :: String -> [String]
splitBits = unfoldr (\x -> if null x then Nothing else Just $ splitAt (nextBits x) x)
    where nextBits = (+ 1) . (* 2) . length . takeWhile (== '0')

binToDec :: [Int] -> Int
binToDec = sum . map (2 ^) . elemIndices 1 . reverse
