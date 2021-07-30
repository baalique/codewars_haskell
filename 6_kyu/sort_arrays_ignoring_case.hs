-- https://www.codewars.com/kata/51f41fe7e8f176e70d0002b9

module Sort where

import           Data.Char                      ( toLower )
import           Data.List                      ( sortBy )

sortme :: [String] -> [String]
sortme = sortBy (\a b -> compare (lower a) (lower b)) where lower = map toLower
