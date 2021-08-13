-- https://www.codewars.com/kata/56dbe0e313c2f63be4000b25

module Codewars.G964.Opstrings1 where

import           Data.List                      ( intercalate )
import           Data.List.Split                ( splitOn )

vertMirror :: String -> String
vertMirror = reconstruct $ map reverse

horMirror :: String -> String
horMirror = reconstruct reverse

oper :: (String -> String) -> String -> String
oper f = f

reconstruct :: ([String] -> [String]) -> String -> String
reconstruct f = intercalate "\n" . f . splitOn "\n"
