-- https://www.codewars.com/kata/57814d79a56c88e3e0000786

module AlternateSplit.JorgeVS.Kata where

encrypt :: String -> Int -> String
encrypt str n | n <= 0 = str
encrypt str n          = encrypt (oddChars ++ evenChars) (n - 1)
 where
  evenChars = filterChars even
  oddChars  = filterChars odd
  filterChars f = [ str !! idx | idx <- [0 .. length str - 1], f idx ]

decrypt :: String -> Int -> String
decrypt str n | n <= 0 = str
decrypt str n          = decrypt getNext (n - 1)
 where
  left  = getHalf take
  right = getHalf drop
  getHalf f = f (length str `div` 2) str
  getNext = reverse $ getNext' left right []
  getNext' []       []       acc = acc
  getNext' xs       []       acc = head xs : acc
  getNext' []       ys       acc = head ys : acc
  getNext' (x : xs) (y : ys) acc = getNext' xs ys (x : y : acc)
  