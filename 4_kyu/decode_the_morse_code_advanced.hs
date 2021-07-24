-- https://www.codewars.com/kata/54b72c16cd7f5154e9000457

module Kata.DecodeMorseAdvanced where

import           Data.List                      ( intercalate )
import           Data.List.Split
import           Kata.DecodeMorseAdvanced.Preload

decodeBits :: String -> String
decodeBits bits = (intercalate "" . map convertToMorse) (group $ every len trimmedBits)
 where
  len         = getMinLen trimmedBits
  trimmedBits = trim '0' bits

decodeMorse :: String -> String
decodeMorse morse = unwords $ map getWord morseWords
 where
  getWord xs = intercalate "" $ map (morseCodes !) (words xs)
  morseWords = splitOn "   " morse

convertToMorse :: String -> String
convertToMorse "0"       = ""
convertToMorse "1"       = "."
convertToMorse "000"     = " "
convertToMorse "111"     = "-"
convertToMorse "0000000" = "   "
convertToMorse _         = undefined

group :: String -> [String]
group xs = group' xs ([], [])
 where
  group' [] (cur, acc)                      = reverse $ cur : acc
  group' (x : xs) (cur, acc) | x `elem` cur = group' xs (x : cur, acc)
  group' (x : xs) ("" , acc)                = group' xs ([x], acc)
  group' (x : xs) (cur, acc)                = group' xs ([x], cur : acc)

every :: Int -> [a] -> [a]
every len xs = case drop (len - 1) xs of
  y : ys -> y : every len ys
  []     -> []

getMinLen :: String -> Int
getMinLen = minimum . map length . group

trim :: Char -> String -> String
trim p = reverse . trimPrefix p . reverse . trimPrefix p where trimPrefix prefix = dropWhile (== prefix)
