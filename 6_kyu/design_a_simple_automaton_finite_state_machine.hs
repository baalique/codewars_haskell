-- https://www.codewars.com/kata/5268acac0d3f019add000203

module Automaton where

data State = Q1 | Q2 | Q3 deriving Eq

readCommands :: [Char] -> Bool
readCommands = (Q2 ==) . nextState Q1

nextState :: State -> [Char] -> State
nextState q  []         = q
nextState Q1 ('0' : xs) = nextState Q1 xs
nextState Q1 ('1' : xs) = nextState Q2 xs
nextState Q2 ('0' : xs) = nextState Q3 xs
nextState Q2 ('1' : xs) = nextState Q2 xs
nextState Q3 (_   : xs) = nextState Q2 xs
nextState _  _          = undefined
