-- https://www.codewars.com/kata/5274e122fc75c0943d000148

module Codewars.Commas where

import           Data.List                      ( intercalate )
import           Data.List.Split                ( chunksOf )

groupByCommas :: Int -> String
groupByCommas = intercalate "," . map reverse . reverse . chunksOf 3 . reverse . show
