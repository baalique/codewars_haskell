-- https://www.codewars.com/kata/571bff6082661c8a11000823

module Codewars.Kirilloid.Permutations where

import           Data.List                      ( group
                                                , sort
                                                )

perms :: Ord a => [a] -> Integer
perms xs = factSeq xs `div` product (map factSeq (group $ sort xs))
    where factSeq xs = product [1 .. (fromIntegral $ length xs)]
