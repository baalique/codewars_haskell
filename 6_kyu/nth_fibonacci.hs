-- https://www.codewars.com/kata/522551eee9abb932420004a0

module Fib where

fib :: Int -> Integer
fib = (fibs !!) . pred where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
