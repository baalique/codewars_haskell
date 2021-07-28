-- https://www.codewars.com/kata/51689e27fe9a00b126000004

module FormatSentence where

formatWords :: [String] -> String
formatWords = formatWords' . filter (/= "")
  where
    formatWords' []       = ""
    formatWords' [x]      = x
    formatWords' [x, y]   = x ++ " and " ++ y
    formatWords' (x : xs) = x ++ ", " ++ formatWords xs
