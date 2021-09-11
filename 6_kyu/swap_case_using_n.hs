-- https://www.codewars.com/kata/5f3afc40b24f090028233490

module SwapCaseUsingN where

import           Data.Char                      ( isLetter
                                                , isLower
                                                , toLower
                                                , toUpper
                                                )

swap :: [Char] -> Int -> [Char]
swap str n = reverse $ doSwap str (cycle $ toBin n) []

doSwap :: [Char] -> [Int] -> [Char] -> [Char]
doSwap [] _ acc = acc
doSwap (x : xs) (n : ns) acc | isLetter x = doSwap xs ns ((if n == 1 then swapCase x else x) : acc)
                             | otherwise  = doSwap xs (n : ns) (x : acc)
doSwap _ _ _ = undefined

swapCase :: Char -> Char
swapCase c = if isLower c then toUpper c else toLower c

toBin :: Int -> [Int]
toBin 0 = [0]
toBin x = reverse $ toBin' x
  where
    toBin' 0 = []
    toBin' x | x `mod` 2 == 1 = 1 : toBin' (x `div` 2)
             | otherwise      = 0 : toBin' (x `div` 2)
