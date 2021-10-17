-- https://www.codewars.com/kata/5b33452ab6989d2407000100

module AntiPalindromes where

countPal :: Int -> (Int, Int)
countPal 1 = (9, 9)
countPal 2 = (9, 18)
countPal n = (withEqualLength, total)
  where
    d               = (n + 1) `div` 2
    withEqualLength = 9 * 10 ^ (d - 1)
    total           = read $ "1" ++ (if even n then "9" else "0") ++ replicate (d - 2) '9' ++ "8"
