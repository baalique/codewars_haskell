-- https://www.codewars.com/kata/534ffb35edb1241eda0015fe

module BlackJack where

scoreHand :: [String] -> Int
scoreHand xs = getBlackJack $ foldl (flip addCard) [0] xs

addCard :: String -> [Int] -> [Int]
addCard "2"  = map (+ 2)
addCard "3"  = map (+ 3)
addCard "4"  = map (+ 4)
addCard "5"  = map (+ 5)
addCard "6"  = map (+ 6)
addCard "7"  = map (+ 7)
addCard "8"  = map (+ 8)
addCard "9"  = map (+ 9)
addCard "10" = map (+ 10)
addCard "J"  = map (+ 10)
addCard "Q"  = map (+ 10)
addCard "K"  = map (+ 10)
addCard "A"  = \xs -> (++) (map (+ 1) xs) (map (+ 11) xs)
addCard _    = undefined

getBlackJack :: [Int] -> Int
getBlackJack xs = if any (<= 21) xs then maximum (filter (<= 21) xs) else minimum xs
