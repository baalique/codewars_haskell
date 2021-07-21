-- https://www.codewars.com/kata/551f23362ff852e2ab000037

module PyramidSlideDown where


longestSlideDown :: [[Int]] -> Int
longestSlideDown pyramid = maximum (last (getPyramid pyramid 0 []))

getPyramid :: [[Int]] -> Int -> [[Int]] -> [[Int]]
getPyramid []       _   acc = acc
getPyramid (x : xs) row acc = getPyramid xs (row + 1) (acc ++ [getRow acc x row])

getRow :: [[Int]] -> [Int] -> Int -> [Int]
getRow pyramid xs 0   = xs
getRow pyramid xs row = zipWith (getMaxValue pyramid row) xs [0 ..]

getMaxValue :: [[Int]] -> Int -> Int -> Int -> Int
getMaxValue pyramid 0 _ _ = head (head pyramid)
getMaxValue pyramid row val idx
    | idx == 0                             = head (pyramid !! (row - 1)) + val
    | idx == length (pyramid !! (row - 1)) = last (pyramid !! (row - 1)) + val
    | otherwise                            = max (pyramid !! (row - 1) !! idx) (pyramid !! (row - 1) !! (idx - 1)) + val
