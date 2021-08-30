-- https://www.codewars.com/kata/5938f5b606c3033f4700015a

module AlphabetWar where

import           Data.List                      ( elemIndices
                                                , group
                                                , sort
                                                )

alphabetWar :: String -> [Char]
alphabetWar = getWinner . explode

explode :: String -> String
explode str = [ str !! idx | idx <- [0 .. length str - 1], idx `notElem` removeIndices str ]
 where
  dedup         = map head . group . sort
  getAdj        = concatMap (\i -> [i - 1, i, i + 1]) . elemIndices '*'
  removeIndices = \s -> dedup . filter (\i -> i >= 0 && i < length s) . getAdj $ s


getWinner :: String -> String
getWinner str = case getWinner' str of
  x | x < 0 -> "Left side wins!"
  x | x > 0 -> "Right side wins!"
  _         -> "Let's fight again!"

getWinner' :: String -> Int
getWinner' []       = 0
getWinner' (x : xs) = getWinner' xs + countScores x

countScores :: Char -> Int
countScores 'w' = -4
countScores 'p' = -3
countScores 'b' = -2
countScores 's' = -1
countScores 'm' = 4
countScores 'q' = 3
countScores 'd' = 2
countScores 'z' = 1
countScores _   = 0
