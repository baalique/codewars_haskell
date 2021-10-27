-- https://www.codewars.com/kata/569d488d61b812a0f7000015

module Codewars.Kata.DataReverse where

import Data.List.Split (chunksOf)

dataReverse :: [Int] -> [Int]
dataReverse = concat . reverse . chunksOf 8
