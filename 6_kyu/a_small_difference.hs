-- https://www.codewars.com/kata/580b746830f829e46400001e

module ASmallDifference where

aSmallDifference :: String -> String -> Bool
aSmallDifference s1 s2 = (== 1) $ levenshtein s1 s2

levenshtein :: [Char] -> [Char] -> Int
levenshtein s1 s2 = distance (length s1) (length s2)
  where
    distance 0 0 = 0
    distance i 0 = i
    distance 0 j = j
    distance i j = minimum
        [ distance i (j - 1) + 1
        , distance (i - 1) j + 1
        , distance (i - 1) (j - 1) + (if s1 !! (i - 1) == s2 !! (j - 1) then 0 else 1)
        ]
