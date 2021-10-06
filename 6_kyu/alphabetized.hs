-- https://www.codewars.com/kata/5970df092ef474680a0000c9

module Alphabetized.Kata where

import           Data.Char                      ( isAlpha
                                                , toLower
                                                )
import           Data.List                      ( sortBy )

alphabetized :: [Char] -> [Char]
alphabetized = sortBy (\c1 c2 -> compare (toLower c1) (toLower c2)) . filter isAlpha
