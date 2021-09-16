-- https://www.codewars.com/kata/5a91e0793e9156ccb0003f6e

module Matrixfy where

import           Data.List.Split                ( chunksOf )

matrixfy :: String -> Either String [[Char]]
matrixfy "" = Left "name must be at least one letter"
matrixfy s  = Right $ chunksOf len $ s ++ drop (length s) (replicate (len ^ 2) '.')
    where len = (ceiling . sqrt . fromIntegral . length) s
