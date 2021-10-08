-- https://www.codewars.com/kata/56b7a75cbd06e6237000138b

module InnerJoin where

innerJoin :: (Eq a) => [[Maybe a]] -> [[Maybe a]] -> Int -> Int -> [[Maybe a]]
innerJoin t1 t2 idx1 idx2 = t1 >>= joinRow t2 idx1 idx2

joinRow :: Eq a => [[Maybe a]] -> Int -> Int -> [Maybe a] -> [[Maybe a]]
joinRow t2 idx1 idx2 row = map (row ++) $ filter (\r -> cmp (row !! idx1) (r !! idx2)) t2
  where
    cmp :: Eq a => Maybe a -> Maybe a -> Bool
    cmp Nothing   _         = False
    cmp _         Nothing   = False
    cmp (Just x1) (Just x2) = x1 == x2
