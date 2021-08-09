-- https://www.codewars.com/kata/54bb6f887e5a80180900046b

module Codewars.Kata.LongestPalindrome where

longestPalindrome :: Eq a => [a] -> Int
longestPalindrome xs = checkPalindrome xs (length xs)

checkPalindrome :: Eq a => [a] -> Int -> Int
checkPalindrome xs len | any isPalindrome (getAllSubsequences xs len) = len
                       | otherwise = checkPalindrome xs (len - 1)

getAllSubsequences :: [a] -> Int -> [[a]]
getAllSubsequences xs len = [ slice xs start (start + len) | start <- [0 .. length xs - len] ]

slice :: [a] -> Int -> Int -> [a]
slice xs start stop = drop start (take stop xs)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x
