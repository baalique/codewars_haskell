-- https://www.codewars.com/kata/5765870e190b1472ec0022a2

module Pathfinder1 where

import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )

pathFinder :: String -> Bool
pathFinder maze = move splittedMaze [(0, 0)] [] (length splittedMaze - 1, length (head splittedMaze) - 1)
    where splittedMaze = splitOn "\n" maze

move :: [String] -> [(Int, Int)] -> [(Int, Int)] -> (Int, Int) -> Bool
move maze [] visited goal                         = False
move maze stack visited goal | head stack == goal = True
move maze stack visited goal                      = move maze (tail stack ++ nextPoints) (head stack : visited) goal
  where
    possiblePoints = filter (\(x, y) -> maze !! x !! y /= 'W') $ getAdjacentPoints maze (head stack)
    nextPoints     = filter (\p -> p `notElem` visited && p `notElem` stack) possiblePoints

getAdjacentPoints :: [[a]] -> (Int, Int) -> [(Int, Int)]
getAdjacentPoints maze (x, y) | x <= 0 && y <= 0               = [(x, y + 1), (x + 1, y)]
                              | x <= 0 && y >= length maze - 1 = [(x, y - 1), (x + 1, y)]
                              | x >= length (head maze) - 1 && y <= 0 = [(x - 1, y), (x, y + 1)]
                              | x >= length (head maze) - 1 && y >= length maze - 1 = [(x - 1, y), (x, y - 1)]
                              | x <= 0                         = [(x, y - 1), (x, y + 1), (x + 1, y)]
                              | x >= length (head maze) - 1    = [(x, y - 1), (x, y + 1), (x - 1, y)]
                              | y <= 0                         = [(x - 1, y), (x + 1, y), (x, y + 1)]
                              | y >= length maze - 1           = [(x - 1, y), (x + 1, y), (x, y - 1)]
                              | otherwise                      = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
