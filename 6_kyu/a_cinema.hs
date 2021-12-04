-- https://www.codewars.com/kata/603301b3ef32ea001c3395d0

module Cinema where

cinema :: Int -> Int -> Maybe [Char]
cinema b g =
    let x        = max b g
        y        = min b g
        (xc, yc) = if b > g then ('B', 'G') else ('G', 'B')
    in  if x > 2 * y then Nothing else Just $ getSeats x y xc yc

getSeats :: Int -> Int -> Char -> Char -> [Char]
getSeats x y xc yc =
    let r = x - y
        s = 2 * y - x
    in  concat $ replicate r [xc, yc, xc] ++ replicate s [xc, yc]
