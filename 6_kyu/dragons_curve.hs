-- https://www.codewars.com/kata/53ad7224454985e4e8000eaa

module DragonCurve where

import           Data.List                      ( elemIndices )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )

dragon :: Int -> String
dragon n | n < 0 = ""
dragon n         = filter (`notElem` "ab") . foldl (const . replaceRule) "Fa" . enumFromTo 1 $ n

replaceAll :: M.Map Char [Char] -> [Char] -> [Char]
replaceAll rules = foldl (\acc x -> if x `M.member` rules then acc ++ (rules M.! x) else acc ++ [x]) []

replaceRule :: [Char] -> [Char]
replaceRule = replaceAll $ M.fromList [('a', "aRbFR"), ('b', "LFaLb")]
