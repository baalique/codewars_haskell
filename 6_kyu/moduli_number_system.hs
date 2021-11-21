-- https://www.codewars.com/kata/54db15b003e88a6a480000b9

module Codewars.Kata.ModSys where

import           Data.List                      ( intercalate )

fromNb2Str :: Integer -> [Integer] -> String
fromNb2Str = (convertToStr .) . getMod

getMod :: Integer -> [Integer] -> Maybe [Integer]
getMod n xs | (not . areCoPrime) xs = Nothing
            | n >= product xs       = Nothing
            | otherwise             = Just $ map (mod n) xs

convertToStr :: Maybe [Integer] -> String
convertToStr Nothing   = "Not applicable"
convertToStr (Just xs) = "-" ++ intercalate "--" (map show xs) ++ "-"

areCoPrime :: [Integer] -> Bool
areCoPrime xs = all ((== 1) . uncurry gcd) [ (x, y) | x <- xs, y <- xs, x /= y ]
