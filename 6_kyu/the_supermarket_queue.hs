-- https://www.codewars.com/kata/57b06f90e298a7b53d000a86

module SupermarketQueue where

queueTime :: [Int] -> Int -> Int
queueTime []    _     = 0
queueTime queue tills = getQueueTime (drop tills queue) (take tills queue) 0

getQueueTime :: [Int] -> [Int] -> Int -> Int
getQueueTime []    tills acc = acc + maximum tills
getQueueTime queue tills acc = getQueueTime nextQueue nextTills (acc + 1)
    where (nextQueue, nextTills) = calcNextMinute queue tills (length tills)

calcNextMinute :: [Int] -> [Int] -> Int -> ([Int], [Int])
calcNextMinute queue              tills              0 = (queue, map (\x -> x - 1) tills)
calcNextMinute (queueX : queueXs) (0 : tillsXs)      c = calcNextMinute queueXs (tillsXs ++ [queueX]) (c - 1)
calcNextMinute queue              []                 _ = (queue, [])
calcNextMinute queue              (tillsX : tillsXs) c = calcNextMinute queue (tillsXs ++ [tillsX]) (c - 1)
