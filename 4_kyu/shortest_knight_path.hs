-- https://www.codewars.com/kata/549ee8b47111a81214000941

module ShortestKnightPath.Kata where

import           Control.Monad                  ( liftM2 )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )

type XY = (Int, Int)

knight :: String -> String -> Int
knight s1 s2 = move stop [[start]]
  where
    start = convertCoords s1
    stop  = convertCoords s2

move :: XY -> [[XY]] -> Int
move stop paths = if any ((== stop) . head) paths then length (head paths) - 1 else move stop (paths >>= nextMoves)

nextMoves :: [XY] -> [[XY]]
nextMoves xs@((x, y) : _) = [ point : xs | point <- filter (liftM2 (&&) isInside (`notElem` xs)) possibleMoves ]
    where possibleMoves = [ (x + dx, y + dy) | dx <- [-2, -1, 1, 2], dy <- [-2, -1, 1, 2], abs dx * abs dy == 2 ]
nextMoves _ = undefined

isInside :: XY -> Bool
isInside (x, y) = x >= 0 && y >= 0 && x <= 7 && y <= 7

convertCoords :: [Char] -> XY
convertCoords [l, n] = (f l letters, f n numbers) where f c abc = fromJust $ elemIndex c abc
convertCoords _      = undefined

letters :: [Char]
letters = ['a' .. 'h']

numbers :: [Char]
numbers = ['1' .. '8']
