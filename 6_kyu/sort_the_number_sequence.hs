-- https://www.codewars.com/kata/5816b76988ca9613cc00024f

module SortTheNumberSequence where

import           Data.List                      ( null
                                                , sort
                                                , sortBy
                                                )
import           Data.List.Split                ( splitWhen )

sortSequence :: [Word] -> [Word]
sortSequence = tail . concat . sortBy (\a b -> compare (sum a) (sum b)) . map ((++ [0]) . sort) . splitWhen (== 0)
