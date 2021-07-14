-- https://www.codewars.com/kata/5202ef17a402dd033c000009

module TitleCase where

import           Data.Char
import           Data.List

titleCase :: String -> String -> String
titleCase minor title = unwords $ upperFirst $ map doTitle (getLowerWords title)
  where
    upperFirst []       = []
    upperFirst (x : xs) = upperWord x : xs

    minorList = getLowerWords minor

    getLowerWords text = words $ map toLower text

    doTitle word | word `elem` minorList = lowerWord word
    doTitle word                         = upperWord word

    changeCaseWord f []       = ""
    changeCaseWord f (x : xs) = f x : xs

    lowerWord = changeCaseWord toLower
    upperWord = changeCaseWord toUpper
