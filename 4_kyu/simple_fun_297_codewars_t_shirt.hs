-- https://www.codewars.com/kata/591e996f67cd75f274000006

module TShirts where

import           Data.Map                       ( Map
                                                , adjust
                                                , elems
                                                , fromList
                                                )
import           TShirts.Preloaded              ( Colour(Black, Blue, Orange, Purple, Red, White) )

codewarsTshirts :: Int -> [(Colour, Colour)] -> Bool
codewarsTshirts = isEnough . getStorage

isEnough :: Ord k => Map k Int -> [(k, k)] -> Bool
isEnough storage _ | isEmptyStorage storage = False
isEnough storage [] = True
isEnough storage ((x, y) : xs) = isEnough (updateStorage storage x) xs || isEnough (updateStorage storage y) xs

getStorage :: Int -> Map Colour Int
getStorage total = fromList [ (colour, total `div` 6) | colour <- [White, Orange, Blue, Purple, Red, Black] ]

updateStorage :: (Ord k, Num a) => Map k a -> k -> Map k a
updateStorage storage colour = adjust (\x -> x - 1) colour storage

isEmptyStorage :: Map k Int -> Bool
isEmptyStorage = any (< 0) . elems
