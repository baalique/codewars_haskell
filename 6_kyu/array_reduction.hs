-- https://www.codewars.com/kata/5a6761be145c4691ee000090

module ArrayReduction where

solve :: Int -> Int
solve n = sum $ iter [2 .. n] [1]

iter :: [Int] -> [Int] -> [Int]
iter [] acc                          = acc
iter xs@(x : _) acc | x >= length xs = xs ++ acc
iter xs@(x : _) acc                  = iter (removeEvery x xs) (x : acc)

removeEvery :: Int -> [a] -> [a]
removeEvery n = map snd . filter (\(i, _) -> i `mod` n /= 0) . zip [0 ..]
