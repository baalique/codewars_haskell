-- https://www.codewars.com/kata/5894318275f2c75695000146

module Delete where

deleteDigit :: Int -> Int
deleteDigit = maximum . map read . (\xs -> [ take idx xs ++ drop (idx + 1) xs | idx <- [0 .. length xs - 1] ]) . show
