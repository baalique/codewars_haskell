-- https://www.codewars.com/kata/54dc6f5a224c26032800005c

module Codewars.Kata.Bookseller where

import           Codewars.Kata.Bookseller.Types

stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist [] _  = []
stocklist _  [] = []
stocklist st cs = map (\c -> (c, getQuantity c st)) cs

getQuantity :: Char -> [Stock] -> Int
getQuantity c = foldr (\(Stock (lt : lts) amount) acc -> if lt == c then acc + amount else acc) 0
