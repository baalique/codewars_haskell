-- https://www.codewars.com/kata/5a2c22271f7f709eaa0005d3

module SingleCharPalins where

import           Control.Monad                  ( ap )
import           Data.List                      ( inits
                                                , tails
                                                )

solve :: String -> String
solve xs = if isPalindrome xs
    then "OK"
    else case len of
        0 -> "not possible"
        _ -> "remove one"
  where
    isPalindrome = ap (==) reverse
    len          = length . filter isPalindrome . (zipWith (++) <$> inits <*> tail . tails) $ xs
