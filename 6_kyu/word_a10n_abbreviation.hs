-- https://www.codewars.com/kata/5375f921003bf62192000746

module A9n where

import           Data.Char                      ( isAlpha )
import           Data.List                      ( intercalate )

abbreviate :: String -> String
abbreviate = intercalate "" . map makeAbbreviation . splitBySymbols (not . isAlpha)

splitBySymbols :: (Char -> Bool) -> [Char] -> [[Char]]
splitBySymbols f str = reverse . map reverse $ splitBySymbols' f str "" []
  where
    splitBySymbols' _ [] ""  acc = acc
    splitBySymbols' _ [] cur acc = cur : acc
    splitBySymbols' f (x : xs) cur acc | f x       = splitBySymbols' f xs "" ([x] : cur : acc)
                                       | otherwise = splitBySymbols' f xs (x : cur) acc

makeAbbreviation :: String -> String
makeAbbreviation str | length str <= 3 = str
makeAbbreviation str                   = [head str] ++ show (length str - 2) ++ [last str]
