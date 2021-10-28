-- https://www.codewars.com/kata/54b80308488cb6cd31000161

module Codewars.Kata.Groups where

groupCheck :: String -> Bool
groupCheck = check []

check :: [Char] -> [Char] -> Bool
check [] [] = True
check _  [] = False
check stack (x : xs) | x `elem` openList = check (x : stack) xs
check (st : sts) (x : xs) | parensMatch st x = check sts xs
check _ _   = False

openList :: [Char]
openList = "([{"

parensMatch :: Char -> Char -> Bool
parensMatch '(' ')' = True
parensMatch '[' ']' = True
parensMatch '{' '}' = True
parensMatch _   _   = False
