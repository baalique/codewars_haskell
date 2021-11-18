-- https://www.codewars.com/kata/5a4a391ad8e145cdee0000c4

module HasSubPatternII where

import           Data.List                      ( group
                                                , sort
                                                )

hasSubpattern :: String -> Bool
hasSubpattern = (> 1) . foldl1 gcd . map length . group . sort
