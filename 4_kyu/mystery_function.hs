-- https://www.codewars.com/kata/56b2abae51646a143400001d

module MysteryFunction where

import           Data.Bits                      ( shiftR
                                                , xor
                                                )

mystery :: Int -> Int
mystery x = x `xor` (x `shiftR` 1)

mysteryInv :: Int -> Int
mysteryInv x = mysteryInv' x 0
 where
  mysteryInv' 0 i = i
  mysteryInv' n i = mysteryInv' (n `shiftR` 1) (i `xor` n)

nameOfMystery :: String
nameOfMystery = "Gray code"
