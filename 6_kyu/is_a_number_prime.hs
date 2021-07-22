-- https://www.codewars.com/kata/5262119038c0985a5b00029f

module IsPrime where


isPrime :: Integer -> Bool
isPrime x | x <= 3                   = x > 1
isPrime x | even x || x `rem` 3 == 0 = False
isPrime x                            = checkPrime x 5
  where
    checkPrime n d | d ^ 2 > n = True
    checkPrime n d             = not (n `rem` d == 0 || n `rem` (d + 2) == 0) && checkPrime n (d + 6)
