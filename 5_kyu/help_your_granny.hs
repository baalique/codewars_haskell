-- https://www.codewars.com/kata/5536a85b6ed4ee5a78000035

module Codewars.Kata.Granny where

tour :: [String] -> [(String, String)] -> [(String, Double)] -> Integer
tour friends fTowns distTable = floor $ getDistances friendTowns distTable 0 0
  where friendTowns = [ (friend, town) | (friend, town) <- fTowns, friend `elem` friends ]

getDistances []               _         distance lastDistance = distance + lastDistance
getDistances ((_, town) : ts) distTable distance lastDistance = getDistances ts distTable nextDistance currentDistance
 where
  nextDistance    = distance + sqrt (currentDistance ^ 2 - lastDistance ^ 2)
  currentDistance = foldl (\acc (t, val) -> if t == town then val else acc) 0 distTable
