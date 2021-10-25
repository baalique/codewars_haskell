-- https://www.codewars.com/kata/5848565e273af816fb000449

module SimpleEncryption where

import           Data.Char                      ( ord )

encryptThis :: String -> String
encryptThis = unwords . map ((\(x : xs) -> (++ xs) $ show $ ord x) . swapLetters) . words
  where
    swapLetters xs = case length xs of
        len | len > 2 -> [head xs, last xs] ++ [ xs !! idx | idx <- [2 .. length xs - 2] ] ++ [xs !! 1]
        _             -> xs
