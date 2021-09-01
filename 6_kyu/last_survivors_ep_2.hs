-- https://www.codewars.com/kata/60a1aac7d5a5fc0046c89651

module LastSurvivors where

import           Data.List                      ( elemIndex
                                                , elemIndices
                                                )
import           Data.Maybe                     ( fromJust )

lastSurvivors :: String -> String
lastSurvivors str = case getEqualLetters str of
    Nothing -> str
    Just c  -> lastSurvivors $ substituteEqualLetters str c

getEqualLetters :: String -> Maybe Char
getEqualLetters [] = Nothing
getEqualLetters (x : xs) | x `elem` xs = Just x
                         | otherwise   = getEqualLetters xs

substituteEqualLetters :: String -> Char -> String
substituteEqualLetters str c = removeSecond
  where
    subLetter                = moveLetter c 1
    firstIdx : secondIdx : _ = elemIndices c str
    subFirst                 = take firstIdx str ++ [subLetter] ++ drop (firstIdx + 1) str
    removeSecond             = take secondIdx subFirst ++ drop (secondIdx + 1) subFirst

moveLetter :: Char -> Int -> Char
moveLetter chr delta = (!!) abc (mod (idx + delta + length abc) (length abc))
  where
    abc = ['a' .. 'z']
    idx = fromJust $ elemIndex chr abc
