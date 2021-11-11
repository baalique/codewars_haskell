-- https://www.codewars.com/kata/526e8de0512511429e000006

module Trim where

trim :: String -> String
trim = reverse . dropSpace . reverse . dropSpace where dropSpace = dropWhile (`elem` " \n\t\r\v")
