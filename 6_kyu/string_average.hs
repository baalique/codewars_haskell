-- https://www.codewars.com/kata/5966847f4025872c7d00015b

module Kata.StringAverage where

import qualified Data.Map                      as M
import           Data.Tuple                     ( swap )

averageString :: String -> String
averageString xs =
    let
        ys = if all (`elem` map fst nums) $ words xs
            then map (table M.!) . filter (`elem` map fst nums) . words $ xs
            else []
    in  case length ys of
            0 -> "n/a"
            _ -> (revTable M.!) . (\xs -> sum xs `div` length xs) $ ys

table :: M.Map [Char] Int
table = M.fromList nums

revTable :: M.Map Int [Char]
revTable = M.fromList $ map swap nums

nums :: [([Char], Int)]
nums =
    [ ("zero" , 0)
    , ("one"  , 1)
    , ("two"  , 2)
    , ("three", 3)
    , ("four" , 4)
    , ("five" , 5)
    , ("six"  , 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine" , 9)
    ]
