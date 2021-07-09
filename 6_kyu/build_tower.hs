-- https://www.codewars.com/kata/576757b1df89ecf5bd00073b

module Codewars.BuildTower where

buildTower :: Int -> [String]
buildTower n | n < 0 = []
buildTower n         = [ getRow x n | x <- [1 .. n] ]

getRow :: Int -> Int -> String
getRow x total = spaces ++ asterisks ++ spaces
  where
    spaces    = replicate (total - x) ' '
    asterisks = replicate (2 * x - 1) '*'
