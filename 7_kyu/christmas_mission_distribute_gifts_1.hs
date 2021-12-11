-- https://www.codewars.com/kata/584ed874bbf24eb9490001e5

module DistributeGifts.Kata where

distributeGifts :: [Int] -> Int
distributeGifts = sum . map (\x -> if isPrime x then x else head . getFactors $ x)

isPrime :: Int -> Bool
isPrime k = null [ x | x <- [2 .. floor $ sqrt $ fromIntegral k], k `mod` x == 0 ]

getFactors :: Int -> [Int]
getFactors x = [ y | y <- [x `div` 2 + 1, x `div` 2 .. 1], x `mod` y == 0 ]
