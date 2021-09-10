-- https://www.codewars.com/kata/541103f0a0e736c8e40011d5

module Codewars.Kata.Employee where

off :: Int -> [Int]
off n = map snd . filter (not . fst) $ zip (iter 1 (replicate n True)) [1 ..]

iter :: Int -> [Bool] -> [Bool]
iter n xs | n == length xs + 1 = xs
iter n xs                      = iter (n + 1) $ mapEveryNth not n xs

mapEveryNth :: Integral a => (a2 -> a2) -> a -> [a2] -> [a2]
mapEveryNth f = mapEvery f (\c k -> (c + 1) `mod` k == 0)

mapEvery :: Num a1 => (a2 -> a2) -> (a1 -> t -> Bool) -> t -> [a2] -> [a2]
mapEvery f g n xs = mapEvery' f g n xs 0
  where
    mapEvery' _ _ _ [] _ = []
    mapEvery' f g n (x : xs) c | g c n     = f x : mapEvery' f g n xs (c + 1)
                               | otherwise = x : mapEvery' f g n xs (c + 1)
