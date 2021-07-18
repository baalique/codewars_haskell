-- https://www.codewars.com/kata/5259acb16021e9d8a60010af

module LeastCommonMultiple where
import           Prelude                 hiding ( lcm )

lcm :: Integral a => [a] -> a
lcm xs | 0 `elem` xs = 0
       | otherwise   = foldl (\acc x -> abs acc * abs x `div` gcd acc x) 1 xs
