-- https://www.codewars.com/kata/558878ab7591c911a4000007

module Codewars.PigLatin where

import           Data.Char                      ( isAlpha
                                                , toLower
                                                )

pigLatin :: String -> Maybe String
pigLatin xs | not (all isAlpha xs) = Nothing
pigLatin xs                        = Just $ convert $ map toLower xs

convert :: [Char] -> [Char]
convert [] = []
convert xs@(x : _) | x `elem` vowels = xs ++ "way"
                   | otherwise       = dropWhile pr xs ++ takeWhile pr xs ++ "ay"
    where pr = (`notElem` vowels)

vowels :: [Char]
vowels = "aeiou"
