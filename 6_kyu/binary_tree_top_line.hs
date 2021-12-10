-- https://www.codewars.com/kata/5acf6061307eb22aff000162

module TreeTop where

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Eq, Show)

treeTop :: Tree Int -> [Int]
treeTop Nil  = []
treeTop tree = map getValue topLine
  where
    topLine = left ++ [tree] ++ right
    left    = reverse $ tail $ getLeft tree
    right   = tail $ getRight tree

getLeft :: Tree a -> [Tree a]
getLeft Nil              = []
getLeft n@(Node Nil _ _) = [n]
getLeft n@(Node l   _ _) = n : getLeft l

getRight :: Tree a -> [Tree a]
getRight Nil              = []
getRight n@(Node _ _ Nil) = [n]
getRight n@(Node _ _ r  ) = n : getRight r

getValue :: Tree p -> p
getValue (Node _ v _) = v
getValue _            = undefined
