-- https://www.codewars.com/kata/5a49f074b3bfa89b4c00002b

module StringSubpatternRecognitionI where

import           Data.List.Split                ( chunksOf )
import qualified Data.Set                      as S

hasSubpattern :: String -> Bool
hasSubpattern xs = any (((== 1) . length) . S.fromList) (allChunks xs)

allChunks :: [a] -> [[[a]]]
allChunks xs = map (`chunksOf` xs) [1 .. length xs - 1]
