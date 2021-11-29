-- https://www.codewars.com/kata/5a946d9fba1bb5135100007c

module Solution.JorgeVS.Kata where

minimumNumber :: [Integer] -> Integer
minimumNumber xs = nextPrime (sum xs) - sum xs

nextPrime :: Integer -> Integer
nextPrime x = if isPrime x then x else nextPrime (x + 1)

isPrime :: Integer -> Bool
isPrime x | x <= 3                   = x > 1
isPrime x | even x || x `rem` 3 == 0 = False
isPrime x                            = checkPrime x 5
  where
    checkPrime n d | d ^ 2 > n = True
    checkPrime n d             = not (n `rem` d == 0 || n `rem` (d + 2) == 0) && checkPrime n (d + 6)
