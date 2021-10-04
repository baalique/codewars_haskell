-- https://www.codewars.com/kata/54b478163b3d7632d10001c6

module Palindrome where

import           Data.Char                      ( isAlphaNum
                                                , toLower
                                                )

isPalindrome :: [Char] -> Bool
isPalindrome xs = filtered == reverse filtered where filtered = map toLower $ filter isAlphaNum xs
