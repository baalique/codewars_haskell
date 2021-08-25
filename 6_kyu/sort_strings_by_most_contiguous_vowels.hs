-- https://www.codewars.com/kata/5d2d0d34bceae80027bffddb

module ContiguousVowels where

import           Data.List                      ( groupBy
                                                , sortBy
                                                )

sortByVowels :: [String] -> [String]
sortByVowels = map fst . sortBy sortVowels . (`zip` [0 ..])
 where
  sortVowels   = \(x, ix) (y, iy) -> compare (-(getMaxVowels x), ix) (-(getMaxVowels y), iy)
  getMaxVowels = uncurry max . foldr foldVowels (0, 0)
  foldVowels   = \x (curMax, total) -> if isVowel x then (curMax + 1, total) else (0, max curMax total)
  isVowel      = (`elem` vowels)
  vowels       = "aeiouAEIOU"
