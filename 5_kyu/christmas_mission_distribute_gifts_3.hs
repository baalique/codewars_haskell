-- https://www.codewars.com/kata/584e4cfee82520410f000001

module DistributeGifts.Kata where

import           Data.List                      ( sort
                                                , subsequences
                                                )

distributeGifts :: Int -> [Int] -> Either String [Int]
distributeGifts total gifts = getSubsequence $ filter ((== total) . sum) $ subsequences gifts

getSubsequence :: [[Int]] -> Either String [Int]
getSubsequence []      = Left "Mission Failed!"
getSubsequence (x : _) = Right x
