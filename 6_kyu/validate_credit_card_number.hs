-- https://www.codewars.com/kata/5418a1dd6d8216e18a0012b2

module Validate where

import           Data.Char                      ( digitToInt )

validate :: Integer -> Bool
validate =
    (== 0)
        . (`mod` 10)
        . sum
        . map (\x -> if x <= 9 then x else x - 9)
        . zipWith ($) (cycle [id, (* 2)])
        . reverse
        . map digitToInt
        . show
