-- https://www.codewars.com/kata/54ff3102c1bad923760001f3

module Codewars.Kata.Vowel where

getCount :: String -> Int
getCount text = length filtered
  where
    filtered = filter (`elem` vowels) text
    vowels   = "aeiou"
