-- https://www.codewars.com/kata/56bc5f83e8936f3d200009ac

module Math where

import           Prelude                 hiding ( acos
                                                , asin
                                                , atan
                                                , cos
                                                , sin
                                                , tan
                                                )

sin :: Double -> Double
sin x = sum $ take 30 $ zipWith3 (\u pw pr -> u * pw / pr) units (powers alpha) products
  where
    alpha    = x * pi / 180
    units    = cycle [1, -1]
    products = scanl (\acc x -> acc * x * 2 * (x * 2 + 1)) 1 [1 ..]
    powers x = map (x ^) [1, 3 ..]
