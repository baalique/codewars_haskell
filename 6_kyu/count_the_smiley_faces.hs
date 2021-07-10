-- https://www.codewars.com/kata/583203e6eb35d7980400002a

module Smile where

import           Text.Regex.TDFA

countSmileys :: [String] -> Int
countSmileys smiles = length (filter (\x -> x =~ smileRegex) smiles) where smileRegex = "^(:|;)(-|~)?(\\)|D)$"
