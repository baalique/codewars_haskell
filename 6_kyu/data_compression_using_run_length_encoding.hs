-- https://www.codewars.com/kata/578bf2d8daa01a4ee8000046

module Kata.RLE where

import           Data.Char                      ( isAlpha )
import           Data.List                      ( group )
import           Data.List.Split                ( splitWhen )

encode :: String -> String
encode = concatMap (\xs -> show (length xs) ++ [head xs]) . group

decode :: String -> String
decode xs =
    let ns = filter (not . null) . splitWhen isAlpha $ xs
        as = filter (not . null) . splitWhen (not . isAlpha) $ xs
    in  concat $ concat $ zipWith (replicate . read) ns as
