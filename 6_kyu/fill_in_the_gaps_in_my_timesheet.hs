-- https://www.codewars.com/kata/564871e795df155582000013

module Codewars.Kata.FillTheGaps where

import           Data.List                      ( group )

type Task = Maybe Int

fillGaps :: [Task] -> [Task]
fillGaps = concat . reverse . parseGaps [] . group

parseGaps :: Eq a => [[Maybe a]] -> [[Maybe a]] -> [[Maybe a]]
parseGaps acc []       = acc
parseGaps acc [h1]     = h1 : acc
parseGaps acc [h1, h2] = h2 : h1 : acc
parseGaps acc (h1@(Nothing : _) : rest) = parseGaps (h1 : acc) rest
parseGaps acc (h1@(Just x : _) : h2@(Just y : _) : rest) = parseGaps (h1 : acc) (h2 : rest)
parseGaps acc (h1@(Just x : _) : h2@(Nothing : _) : h3@(Just y : _) : rest)
    | x == y = parseGaps ((Just x <$ h2) : h1 : acc) (h3 : rest)
    | x /= y = parseGaps (h2 : h1 : acc) (h3 : rest)
parseGaps _ _ = undefined
