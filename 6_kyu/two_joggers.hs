-- https://www.codewars.com/kata/5274d9d3ebc3030802000165

module Joggers where

nbrOfLaps :: Integer -> Integer -> (Integer, Integer)
nbrOfLaps b c = (x `div` b, x `div` c) where x = lcm b c
