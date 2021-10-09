-- https://www.codewars.com/kata/536c6b8749aa8b3c2600029a

module AStringOfSorts where

import           Data.Function                  ( on )
import           Data.List                      ( elemIndex
                                                , nub
                                                , sortBy
                                                )

sortString :: String -> String -> String
sortString str ptrn = map fst (s1 ++ s2)
 where
  dedupedPattern = nub ptrn
  unsorted       = filter ((`elem` dedupedPattern) . fst) $ zip str [0 ..]
  s1             = sortBy (compare `on` (\(c, _) -> elemIndex c dedupedPattern)) unsorted
  s2             = filter ((`notElem` dedupedPattern) . fst) $ zip str [0 ..]
