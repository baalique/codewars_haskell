-- https://www.codewars.com/kata/57ebdf1c2d45a0ecd7002cd5

module InsideOut where

import           Data.List                      ( intercalate )

insideOut :: String -> String
insideOut = unwords . map insideOutWord . words

insideOutWord :: String -> String
insideOutWord str = intercalate "" $ map (\f -> reverse $ f str) [start, middle, end]
 where
  start s = take (length s `div` 2) s
  middle s = if even $ length s then "" else [s !! (length s `div` 2)]
  end s = drop (length s `div` 2 + (if even $ length s then 0 else 1)) s
  