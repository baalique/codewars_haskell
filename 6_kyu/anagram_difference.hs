-- https://www.codewars.com/kata/5b1b27c8f60e99a467000041

module AnagramDiff where

import           Data.List                      ( group
                                                , sort
                                                )
import qualified Data.Map                      as M

anagramDifference :: [Char] -> [Char] -> Int
anagramDifference xs ys = length xs + length ys - common * 2
 where
  m1     = getOccurencies xs
  m2     = getOccurencies ys
  common = M.foldlWithKey (\acc v len -> acc + (if M.member v m2 then min len (m2 M.! v) else 0)) 0 m1

getOccurencies :: String -> M.Map Char Int
getOccurencies = M.fromList . map (\x -> (head x, length x)) . group . sort
