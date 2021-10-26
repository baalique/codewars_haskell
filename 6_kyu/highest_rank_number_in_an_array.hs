-- https://www.codewars.com/kata/5420fc9bb5b2c7fd57000004

module Term where

import           Control.Arrow                  ( (&&&) )
import           Data.List                      ( group
                                                , sort
                                                , sortOn
                                                )
import           Data.Ord                       ( Down(Down) )

highestRank :: Ord c => [c] -> c
highestRank = head . head . sortOn (Down . (length &&& head)) . group . sort
