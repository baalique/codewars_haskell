-- https://www.codewars.com/kata/5850bff5b76e6599aa00011a

module DistributeGifts.Kata where

distributeGifts :: Int -> [(Int, Int)] -> Int
distributeGifts total = getGifts (replicate (total + 1) 0) total

getGifts :: [Int] -> Int -> [(Int, Int)] -> Int
getGifts xs total gifts = maximum $ fillIndices xs total gifts 1

fillIndices :: [Int] -> Int -> [(Int, Int)] -> Int -> [Int]
fillIndices xs total gifts idx | idx == length gifts + 1 = xs
                               | otherwise               = fillIndices ys total gifts (idx + 1)
    where ys = fillRow xs total gifts idx total

fillRow :: [Int] -> Int -> [(Int, Int)] -> Int -> Int -> [Int]
fillRow xs total gifts idx (-1) = xs
fillRow xs total gifts idx w    = fillRow ys total gifts idx (w - 1)
  where
    ys = if fst (gifts !! (idx - 1)) <= w then take w xs ++ [m] ++ drop (w + 1) xs else xs
    m  = maximum [xs !! w, (xs !! (w - fst (gifts !! (idx - 1)))) + snd (gifts !! (idx - 1))]
