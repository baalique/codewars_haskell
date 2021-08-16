-- https://www.codewars.com/kata/56dbf59b0a10feb08c000227

module Codewars.G964.Opstrings1 where

import           Data.List                      ( intercalate
                                                , transpose
                                                )
import           Data.List.Split                ( splitOn )

rot90Counter :: String -> String
rot90Counter = (!! 3) . iterate rot90Clock

diag2Sym :: String -> String
diag2Sym = diag1Sym . rot

selfieDiag2Counterclock :: String -> String
selfieDiag2Counterclock = intercalate "\n" . zipRows
  where
    zipRows = \s -> zipWith3 (\a b c -> concat [a, "|", b, "|", c]) (f1 s) (f2 s) (f3 s)
    f1      = splitOn "\n"
    f2      = splitOn "\n" . diag2Sym
    f3      = splitOn "\n" . rot90Counter

oper :: (String -> String) -> String -> String
oper = ($)

reconstruct :: ([String] -> [String]) -> String -> String
reconstruct f = intercalate "\n" . f . splitOn "\n"

rot90Clock :: String -> String
rot90Clock = reconstruct (\xs -> [ [ row !! i | row <- reverse xs ] | i <- [0 .. length xs - 1] ])

diag1Sym :: String -> String
diag1Sym = reconstruct transpose

rot :: String -> String
rot = reconstruct $ map reverse . reverse
