-- https://www.codewars.com/kata/609eee71109f860006c377d1

module LastSurvivor where

lastSurvivor :: String -> [Int] -> Char
lastSurvivor str []           = head str
lastSurvivor str (idx : idxs) = lastSurvivor (removeByIndex idx str) idxs

removeByIndex :: Int -> [a] -> [a]
removeByIndex idx xs = take idx xs ++ drop (idx + 1) xs
