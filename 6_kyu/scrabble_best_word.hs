-- https://www.codewars.com/kata/563f960e3c73813942000015

module Codewars.Kata.Scrabble where

import           Data.Char                      ( ord )
import           Data.Function                  ( on )
import           Data.List                      ( maximumBy )

getBestWord :: [Int] -> [String] -> Int
getBestWord = ((fst . maximumBy (compare `on` snd) . reverse . zip [0 ..]) .) . map . countScores

countScores :: [Int] -> [Char] -> Int
countScores = (sum .) . map . countLetter

countLetter :: [Int] -> Char -> Int
countLetter points c = points !! (ord c - ord 'A')
