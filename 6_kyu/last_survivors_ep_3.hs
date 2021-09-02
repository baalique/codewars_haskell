-- https://www.codewars.com/kata/60a2d7f50eee95000d34f414

module LastSurvivors where

import           Data.List                      ( transpose )

lastSurvivors :: [String] -> [Int] -> String
lastSurvivors xs arr = (concat . zipWith drop arr . map (reverse . filter (/= ' ')) . transpose) xs
