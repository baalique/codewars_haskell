-- https://www.codewars.com/kata/56a32dd6e4f4748cc3000006

module Codewars.G964.Rainfall where

import           Data.List.Split                ( splitOn )
import           Data.Map                       ( (!)
                                                , Map
                                                , fromList
                                                , member
                                                )

mean :: String -> String -> Double
mean town = getMean . getTownData town

variance :: String -> String -> Double
variance town = getVariance . getTownData town

getMean :: [Double] -> Double
getMean [] = -1
getMean xs = sum xs / fromIntegral (length xs)

getVariance :: [Double] -> Double
getVariance [] = -1
getVariance xs = (/ fromIntegral (length xs)) . foldr (\x acc -> acc + (x - meanValue) ^ 2) 0 $ xs
    where meanValue = getMean xs

getTownData :: String -> String -> [Double]
getTownData town records = if member town parsedRecords then parsedRecords ! town else []
    where parsedRecords = getRecords records

getRecords :: String -> Map String [Double]
getRecords = fromList . parseRecords
  where
    parseRecords = map ((\[t, d] -> (t, parseData d)) . splitOn ":") . splitOn "\n"
    parseData    = map ((\[_, v] -> read v :: Double) . splitOn " ") . splitOn ","
