-- https://www.codewars.com/kata/53e895e28f9e66a56900011a

module CharacterFrequency where

import           Data.Char                      ( isAlpha
                                                , toLower
                                                )
import           Data.List                      ( group
                                                , sort
                                                , sortBy
                                                )

letterFrequency :: [Char] -> [(Char, Int)]
letterFrequency =
    sortBy (\a b -> compare (-snd a, fst a) (-snd b, fst b))
        . map (\x -> (head x, length x))
        . group
        . sort
        . map toLower
        . filter isAlpha
