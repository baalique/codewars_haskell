-- https://www.codewars.com/kata/56fcc393c5957c666900024d

module Codewars.G964.CoddecSqStrings where

import           Data.List                      ( intercalate )
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )

code :: [Char] -> [Char]
code = intercalate "\n" . rotate 1 . splitOn "\n" . insertNewline . fillWithEmpty
  where
    fillWithEmpty s = s ++ replicate (rowSize s ^ 2 - length s) '\11'
    insertNewline s = intercalate "\n" . chunksOf (rowSize s) $ s
    rowSize = ceiling . sqrt . fromIntegral . length

decode :: [Char] -> [Char]
decode = filter (/= '\11') . intercalate "" . rotate 3 . splitOn "\n"

rotate :: Int -> [[a]] -> [[a]]
rotate times xs = iterate rotateOnce xs !! times

rotateOnce :: [[a]] -> [[a]]
rotateOnce [[]] = [[]]
rotateOnce xs   = [ [ row !! i | row <- reverse xs ] | i <- [0 .. length xs - 1] ]
