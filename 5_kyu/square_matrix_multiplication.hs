-- https://www.codewars.com/kata/5263a84ffcadb968b6000513

module MatrixMul where

import           Data.List                      ( transpose )

type Mat a = [[a]]

matMul :: Num a => Mat a -> Mat a -> Mat a
matMul a b = [ [ sum [ p * q | (p, q) <- zip row col ] | col <- transpose b ] | row <- a ]
