-- https://www.codewars.com/kata/5859aaf04facfeb0d4002051

module Kata.BestParkingSpot where

import           Data.Function                  ( on )
import           Data.List                      ( minimumBy )
import           Preloaded.BestParkingSpot      ( Spot(..) )

bestParkingSpot :: [Spot] -> Int
bestParkingSpot parking = fst $ minimumBy (compare `on` snd) $ reverse routesTable
  where
    routesTable    = zip (map getRouteIndex allRoutes) (map getBestRouteLength allRoutes)
    possibleStarts = [ idx | idx <- [0 .. length parking - 1], parking !! idx == OPEN ]
    allRoutes      = map (map reverse . getAllRoutes parking) possibleStarts

getRouteIndex = head . head

getBestRouteLength :: [[Int]] -> Int
getBestRouteLength routes = minimum $ map getRouteLength routes

getRouteLength :: [Int] -> Int
getRouteLength []       = 0
getRouteLength (x : xs) = snd $ foldl (\(c, t) y -> (y, t + abs (c - y))) (x, 0) xs

getAllRoutes :: [Spot] -> Int -> [[Int]]
getAllRoutes parking start =
    [[start]] >>= nextSpot parking STORE >>= goToStart start >>= nextSpot parking CORRAL >>= goToStart start

nextSpot :: [Spot] -> Spot -> [Int] -> [[Int]]
nextSpot parking goal path = [ p : path | p <- [0 .. length parking - 1], parking !! p == goal ]

goToStart :: a -> [a] -> [[a]]
goToStart start path = [start : path]
