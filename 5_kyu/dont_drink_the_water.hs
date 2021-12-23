-- https://www.codewars.com/kata/562e6df5cf2d3908ad00019e

module DontDrinkTheWater where

import           Data.Function                  ( on )
import           Data.List                      ( elemIndex
                                                , sortBy
                                                )
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromJust )

separateLiquids :: [[Char]] -> [[Char]]
separateLiquids xs =
    chunksOf (length $ head xs) . sortBy (compare `on` (\x -> fromJust (elemIndex x "OAWH"))) . concat $ xs
