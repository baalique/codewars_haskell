-- https://www.codewars.com/kata/58e24788e24ddee28e000053

module SimpleAssembler where

import           Data.Char                      ( isLetter )
import           Data.List.Split                ( splitOn )
import qualified Data.Map.Strict               as M
import           Data.Maybe                     ( fromJust )

type Registers = M.Map String Int

simpleAssembler :: [[Char]] -> Registers
simpleAssembler xs = interpret xs 0 M.empty

interpret :: [[Char]] -> Int -> Registers -> Registers
interpret xs idx m | idx < 0 || idx >= length xs = m
interpret xs idx m                               = interpret xs (moveIdx (xs !! idx) idx m) (moveVal (xs !! idx) m)

moveIdx :: [Char] -> Int -> Registers -> Int
moveIdx str idx m = idx + delta
 where
  instruction : x : y' = splitOn " " str
  y                    = head y'
  delta                = case instruction of
    "jnz" | all isLetter x -> if fromJust (M.lookup x m) /= 0 then getVal y m else 1
          | otherwise      -> if read x /= 0 then getVal y m else 1
    _ -> 1

moveVal :: [Char] -> Registers -> Registers
moveVal str m = newM
 where
  instruction : x : y' = splitOn " " str
  y                    = head y'
  newM                 = case instruction of
    "jnz" -> m
    "inc" -> M.update (\x -> Just (x + 1)) x m
    "dec" -> M.update (\x -> Just (x - 1)) x m
    "mov" -> M.insert x (getVal y m) m
    _     -> undefined

getVal :: String -> Registers -> Int
getVal str m | all isLetter str = fromJust $ M.lookup str m
             | otherwise        = read str
