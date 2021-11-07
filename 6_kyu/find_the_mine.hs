-- https://www.codewars.com/kata/528d9adf0e03778b9e00067e

module MineLocation where

import           Data.Array                     ( Array
                                                , assocs
                                                )

mineLocation :: Array (Int, Int) Int -> Maybe (Int, Int)
mineLocation = Just . fst . head . filter ((== 1) . snd) . assocs
