-- https://www.codewars.com/kata/529872bdd0f550a06b00026e

module LargestProduct where

import           Data.Char                      ( digitToInt )

greatestProduct :: String -> Int
greatestProduct = maximum . map product . filter ((== 5) . length) . getElements . map digitToInt

getElements :: [a] -> [[a]]
getElements []           = []
getElements xs@(_ : xs') = take 5 xs : getElements xs'
