-- https://www.codewars.com/kata/59f7597716049833200001eb

module UpsideDown where

solve :: Int -> Int -> Int
solve a b = (length . filter isUpsideDown) [a .. b - 1]

isUpsideDown :: Int -> Bool
isUpsideDown n = (\s -> all (`elem` "16890") s && rotateVertical s == reverse s) (show n)

rotateVertical :: [Char] -> [Char]
rotateVertical = map getRotation
  where
    getRotation '1' = '1'
    getRotation '6' = '9'
    getRotation '8' = '8'
    getRotation '9' = '6'
    getRotation '0' = '0'
    getRotation _   = undefined
