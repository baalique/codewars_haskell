-- https://www.codewars.com/kata/59325dc15dbb44b2440000af

module Kata.AreWeAlternate where

isAlt :: [Char] -> Bool
isAlt xs = fst $ foldl fld (True, isVowel $ head xs) xs
  where
    fld     = \(res, cur) x -> if not res then (res, cur) else (isVowel x == cur, not cur)
    isVowel = (`elem` "aeiou")
