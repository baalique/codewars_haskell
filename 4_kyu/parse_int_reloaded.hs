-- https://www.codewars.com/kata/525c7c5ab6aecef16e0001a5

module ParseInt where

import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )

parseInt :: String -> Int
parseInt = parseMillion . removeAnd . words . replaceDash
  where
    removeAnd   = filter (/= "and")
    replaceDash = replace '-' " - "

parse :: [Char] -> Int -> ([String] -> Int) -> [[Char]] -> Int
parse _ _ _ [] = 0
parse sep multiplier g xs | sep `elem` xs = multiplier * g (head sp) + g (last sp)
                          | otherwise     = g xs
    where sp = splitBy sep xs

parseMillion :: [String] -> Int
parseMillion = parse "million" 1000000 parseThousand

parseThousand :: [String] -> Int
parseThousand = parse "thousand" 1000 parseHundred

parseHundred :: [String] -> Int
parseHundred = parse "hundred" 100 parseTen

parseTen :: [String] -> Int
parseTen = parse "-" 1 (parseOne . head)

parseOne :: String -> Int
parseOne "zero"      = 0
parseOne "one"       = 1
parseOne "two"       = 2
parseOne "three"     = 3
parseOne "four"      = 4
parseOne "five"      = 5
parseOne "six"       = 6
parseOne "seven"     = 7
parseOne "eight"     = 8
parseOne "nine"      = 9
parseOne "ten"       = 10
parseOne "eleven"    = 11
parseOne "twelve"    = 12
parseOne "thirteen"  = 13
parseOne "fourteen"  = 14
parseOne "fifteen"   = 15
parseOne "sixteen"   = 16
parseOne "seventeen" = 17
parseOne "eightteen" = 18
parseOne "nineteen"  = 19
parseOne "twenty"    = 20
parseOne "thirty"    = 30
parseOne "forty"     = 40
parseOne "fifty"     = 50
parseOne "sixty"     = 60
parseOne "seventy"   = 70
parseOne "eighty"    = 80
parseOne "ninety"    = 90
parseOne _           = 0

splitBy :: String -> [String] -> [[String]]
splitBy p xs = [take idx xs, drop (idx + 1) xs] where idx = (fromJust . elemIndex p) xs

replace :: Char -> String -> String -> String
replace p q = reverse . foldr (\x acc -> if x == p then acc ++ q else acc ++ [x]) []
