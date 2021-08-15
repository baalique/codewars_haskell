-- https://www.codewars.com/kata/56dbeec613c2f63be4000be6

module Codewars.G964.Opstrings1 where

import           Data.List                      ( intercalate
                                                , transpose
                                                )
import           Data.List.Split                ( splitOn )

rot90Clock :: String -> String
rot90Clock = reconstruct (\xs -> [ [ row !! i | row <- reverse xs ] | i <- [0 .. length xs - 1] ])

diag1Sym :: String -> String
diag1Sym = reconstruct transpose

selfieAndDiag1 :: String -> String
selfieAndDiag1 = intercalate "\n" . zipRows
    where zipRows = \s -> zipWith (\a b -> a ++ "|" ++ b) (splitOn "\n" s) (splitOn "\n" . diag1Sym $ s)

oper :: (String -> String) -> String -> String
oper = ($)

reconstruct :: ([String] -> [String]) -> String -> String
reconstruct f = intercalate "\n" . f . splitOn "\n"
