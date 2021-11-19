-- https://www.codewars.com/kata/5a4a2973d8e14586c700000a

module StringSubpatternRecognitionIII where

import           Data.List                      ( group
                                                , sort
                                                )

hasSubpattern :: String -> String
hasSubpattern xs = sort $ concatMap (\x -> take (length x `div` d) x) grouped
  where
    grouped = group . sort $ xs
    d       = foldl1 gcd . map length $ grouped
