-- https://www.codewars.com/kata/54120de842dff35232000195

module NIM where

import           Data.Bits                      ( xor )
import           Data.List                      ( elemIndices )

chooseMove :: [Int] -> (Int, Int)
chooseMove xs = head [ (idx, val) | idx <- [0 .. length xs - 1], val <- [0 .. xs !! idx], isNullNimSum xs (idx, val) ]

isNullNimSum :: [Int] -> (Int, Int) -> Bool
isNullNimSum xs (idx, val) = getNimSum (changeGameState xs (idx, val)) == 0

getNimSum :: [Int] -> Int
getNimSum xs = binToDec $ map bitsToBin $ foldl1 (zipWith xor) $ map (fillWithFalses maxLen) bools
 where
  maxLen = maximum $ map length bools
  bools  = map (map binToBits . decToBin) xs

changeGameState :: [Int] -> (Int, Int) -> [Int]
changeGameState xs (idx, val) = take idx xs ++ [(xs !! idx) - val] ++ drop (idx + 1) xs

binToBits :: Int -> Bool
binToBits 0 = False
binToBits 1 = True
binToBits _ = undefined

bitsToBin :: Bool -> Int
bitsToBin False = 0
bitsToBin True  = 1

fillWithFalses :: Int -> [Bool] -> [Bool]
fillWithFalses n xs = replicate (n - length xs) False ++ xs

decToBin :: Int -> [Int]
decToBin 0 = [0]
decToBin x = reverse $ toBin' x
 where
  toBin' 0 = []
  toBin' x | x `mod` 2 == 1 = 1 : toBin' (x `div` 2)
           | otherwise      = 0 : toBin' (x `div` 2)

binToDec :: [Int] -> Int
binToDec = sum . map (2 ^) . elemIndices 1 . reverse
