-- https://www.codewars.com/kata/60a58520c42fb60055546ce5

module Survivors where

import           Data.List                      ( group
                                                , sort
                                                )

survivors :: [[Char]] -> [Char]
survivors xs = sort $ filter (/= ' ') $ map (\(x, y) -> filled !! y !! x) indicesToRemain
 where
  filled          = fillWithSpaces xs
  indicesToDelete = getAllIndicesToDelete filled
  allIndices      = [ (x, y) | y <- [0 .. length filled - 1], x <- [0 .. length (head filled) - 1] ]
  indicesToRemain = filter (`notElem` indicesToDelete) allIndices

fillWithSpaces :: [[Char]] -> [[Char]]
fillWithSpaces xs = map (fillSpaces $ maximum $ map length xs) xs
  where fillSpaces len ys = ys ++ replicate (len - length ys) ' '

getAdjacentPoints :: (Int, Int) -> [(Int, Int)]
getAdjacentPoints (x, y) = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

getAreaAround :: Int -> (Int, Int) -> [(Int, Int)]
getAreaAround r (x, y) = [ (i, j) | i <- [x - r .. x + r], j <- [y - r .. y + r] ]

getAllIndicesToDelete :: [[Char]] -> [(Int, Int)]
getAllIndicesToDelete xs = (filter (isCoordsIn xs) . dedup . concat . concat) indicesToDelete
 where
  indicesToDelete = [ [ getIndicesToDelete xs (y, x) | y <- [0 .. length (head xs) - 1] ] | x <- [0 .. length xs - 1] ]

getIndicesToDelete :: [[Char]] -> (Int, Int) -> [(Int, Int)]
getIndicesToDelete xs (x, y) = filter (`notElem` groupIndices) areaAround
 where
  radius       = length groupIndices
  groupIndices = getGroupIndices xs (x, y)
  areaAround   = getAreaAround radius (x, y)

getGroupIndices :: [[Char]] -> (Int, Int) -> [(Int, Int)]
getGroupIndices xs (x, y) = getGroupIndices' xs [(x, y)] []
 where
  getGroupIndices' _ [] visited     = visited
  getGroupIndices' xs ((x, y) : _) visited | xs !! y !! x == ' ' = []
  getGroupIndices' xs stack visited = getGroupIndices' xs (tail stack ++ possiblePoints) ((q, p) : visited)
   where
    (q, p)         = head stack
    possiblePoints = filter filterRule $ getAdjacentPoints (q, p)
    filterRule     = \c@(i, j) -> c `notElem` visited && c `notElem` stack && isCoordsIn xs c && xs !! j !! i /= ' '

nextCoords :: [[Char]] -> [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
nextCoords xs visited (x, y) = filter filterRule $ getAdjacentPoints (x, y)
  where filterRule = \c@(i, j) -> c `notElem` visited && isCoordsIn xs c && xs !! j !! i /= ' '

isCoordsIn :: [[a]] -> (Int, Int) -> Bool
isCoordsIn xs (x, y) = x >= 0 && y >= 0 && x < length (head xs) && y < length xs

dedup :: (Eq a, Ord a) => [a] -> [a]
dedup = map head . group . sort
