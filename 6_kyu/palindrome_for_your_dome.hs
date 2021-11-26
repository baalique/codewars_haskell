-- https://www.codewars.com/kata/53046ceefe87e4905e00072a

module Codewars.Kata.Palindrome where

import           Data.Char                      ( isAlphaNum
                                                , toLower
                                                )
import           Prelude                 hiding ( reverse )

isPalindrome :: String -> Bool
isPalindrome xs = reverse ys == ys where ys = map toLower $ filter isAlphaNum xs

reverse :: [a] -> [a]
reverse = rev []
  where
    rev a []       = a
    rev a (x : xs) = rev (x : a) xs
