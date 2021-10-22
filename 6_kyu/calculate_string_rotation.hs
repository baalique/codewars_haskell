-- https://www.codewars.com/kata/5596f6e9529e9ab6fb000014

module Codewars.Kata.Rotation where

shiftedDiff :: String -> String -> Int
shiftedDiff = checkShift 0

checkShift :: Eq a => Int -> [a] -> [a] -> Int
checkShift s a b | b == shift a s = s
                 | s > length a   = -1
                 | otherwise      = checkShift (s + 1) a b

shift :: [a] -> Int -> [a]
shift str n = drop (length str - n) str ++ take (length str - n) str
