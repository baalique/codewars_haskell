-- https://www.codewars.com/kata/56eb16655250549e4b0013f4

module Kata where

import           Data.Function                  ( on )
import           Data.List                      ( elemIndex
                                                , group
                                                , groupBy
                                                , sort
                                                , sortBy
                                                )
import           Data.Time                      ( DayOfWeek(..)
                                                , addDays
                                                , dayOfWeek
                                                , fromGregorian
                                                , toGregorian
                                                )

mostFrequentDays :: Integer -> [String]
mostFrequentDays =
    sortBy (compare `on` (`elemIndex` map show [Monday ..]))
        . map head
        . head
        . groupBy (\a b -> length a == length b)
        . sortBy (flip compare `on` length)
        . group
        . sort
        . map show
        . getDaysOfWeek

getDaysOfWeek :: Integer -> [DayOfWeek]
getDaysOfWeek year = map dayOfWeek $ filter (\d -> (\(y, _, _) -> y) (toGregorian d) == year) days
    where days = scanl (\acc x -> addDays 1 acc) (fromGregorian year 1 1) [0 .. 365]
