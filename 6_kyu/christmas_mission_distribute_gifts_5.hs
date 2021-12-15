-- https://www.codewars.com/kata/584f7b27d8912ab46e0000d5

module DistributeGifts.Kata where

import           Data.List                      ( elemIndex )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( isJust )

type XY = (Int, Int)

distributeGifts :: String -> Either String Int
distributeGifts str = case start of
    Nothing -> Left "Where is Santa Claus?"
    Just _  -> Right distance
  where
    distance = snd $ foldl (\(p, t) x -> (x, t + getDistance p x)) (start, 0) points
    start    = getPoint grid 's'
    points   = filter isJust $ map (getPoint grid) ['A' .. 'Z']
    grid     = filter (not . null) $ splitOn "\n" str

getDistance :: Maybe XY -> Maybe XY -> Int
getDistance (Just (x1, y1)) (Just (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)
getDistance _               _               = 0

getPoint :: Eq a => [[a]] -> a -> Maybe XY
getPoint xs c = (`divMod` length (head xs)) <$> elemIndex c (concat xs)
