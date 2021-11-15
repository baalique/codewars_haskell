-- https://www.codewars.com/kata/5326ef17b7320ee2e00001df

module EscapeTheMines where

import           Data.List                      ( transpose )

type XY = (Int, Int)
type Grid = [[Bool]]
data Move = U | D | R | L deriving (Eq, Show)

solve :: Grid -> XY -> XY -> [Move]
solve grid start stop = convertMoves $ reverse $ head $ move (transpose grid) stop [[start]]

move :: Grid -> XY -> [[XY]] -> [[XY]]
move _ stop paths | any ((== stop) . head) paths = filter ((== stop) . head) paths
move grid stop paths                             = move grid stop (paths >>= nextMoves grid)

nextMoves :: Grid -> [XY] -> [[XY]]
nextMoves grid xs = [ point : xs | point <- filter (isPossible grid xs) $ getAdjacentPoints grid $ head xs ]

convertMoves :: [XY] -> [Move]
convertMoves (x : rest@(y : _)) = getMove x y : convertMoves rest
convertMoves _                  = []

getMove :: XY -> XY -> Move
getMove (x1, y1) (x2, y2) | x1 - x2 == 1  = L
getMove (x1, y1) (x2, y2) | x1 - x2 == -1 = R
getMove (x1, y1) (x2, y2) | y1 - y2 == 1  = U
getMove (x1, y1) (x2, y2) | y1 - y2 == -1 = D
getMove _ _                               = undefined

isFree :: Grid -> XY -> Bool
isFree grid (x, y) = grid !! y !! x

isPossible :: Grid -> [XY] -> XY -> Bool
isPossible grid xs x = isFree grid x && x `notElem` xs

getAdjacentPoints :: [[a]] -> (Int, Int) -> [XY]
getAdjacentPoints grid (x, y) = filter (isInside grid) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

isInside :: [[a]] -> XY -> Bool
isInside grid (x, y) = x >= 0 && y >= 0 && x < length (head grid) && y < length grid
