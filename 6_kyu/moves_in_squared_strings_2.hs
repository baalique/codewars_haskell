-- https://www.codewars.com/kata/56dbe7f113c2f63570000b86

module Codewars.G964.Opstrings2 where

import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )

rot :: String -> String
rot = reconstruct $ map reverse . reverse

selfieAndRot :: String -> String
selfieAndRot str = fill str ++ "\n" ++ rot (fill str)
    where fill = reconstruct $ map (\r -> r ++ replicate (length r) '.')

oper :: (String -> String) -> String -> String
oper = ($)

reconstruct :: ([String] -> [String]) -> String -> String
reconstruct f = intercalate "\n" . f . splitOn "\n"
