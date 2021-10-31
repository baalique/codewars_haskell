-- https://www.codewars.com/kata/58d76854024c72c3e20000de

module ReverseEveryOtherWord where

import           Data.List.Split                ( splitOn )

reverseEveryOther :: String -> String
reverseEveryOther = unwords . zipWith ($) (cycle [id, reverse]) . filter (not . null) . splitOn " "
