-- https://www.codewars.com/kata/54de3257f565801d96001200

module Codewars.G964.Stockbroker where

import           Data.Char                      ( isSpace )
import           Data.List                      ( dropWhileEnd
                                                , intercalate
                                                )
import           Data.List.Split                ( splitOn )
import           Text.Regex.TDFA                ( (=~)
                                                , Regex
                                                , RegexMaker(makeRegex)
                                                )

data Status = B | S deriving (Eq, Read)
data Order = Order
    { name     :: String
    , quantity :: Int
    , price    :: Double
    , status   :: Status
    }
    | BadlyFormed {badOrder :: String}

balanceStatements :: String -> String
balanceStatements ""  = "Buy: 0 Sell: 0"
balanceStatements str = "Buy: " ++ show buy ++ " Sell: " ++ show sell ++ badlyFormed
  where
    orders       = map (parseOrder . trimSpaces) (splitOn "," str)
    properOrders = filter isProperOrder orders
    badOrders    = filter (not . isProperOrder) orders
    buyOrders    = filter (\o -> status o == B) properOrders
    sellOrders   = filter (\o -> status o == S) properOrders
    buy          = sum $ map getTotal buyOrders
    sell         = sum $ map getTotal sellOrders
    badlyLength  = length badOrders
    badlyFormed  = case badlyLength of
        0 -> ""
        _ -> "; Badly formed " ++ show badlyLength ++ ": " ++ intercalate " ;" (map badOrder badOrders) ++ " ;"

parseOrder :: String -> Order
parseOrder order | not (order =~ orderRegex) = BadlyFormed order
                 | otherwise                 = getOrder $ splitOn " " order

getOrder :: [String] -> Order
getOrder [n, q, p, s] =
    Order { name = n, quantity = read q :: Int, price = read p :: Double, status = read s :: Status }
getOrder _ = undefined

isProperOrder :: Order -> Bool
isProperOrder Order{}         = True
isProperOrder (BadlyFormed _) = False

getTotal :: Order -> Int
getTotal (Order _ q p _) = round $ fromIntegral q * p
getTotal _               = undefined

trimSpaces :: [Char] -> [Char]
trimSpaces = dropWhileEnd isSpace . dropWhile isSpace

orderRegex :: [Char]
orderRegex = "^[A-Z0-9\\.]+ [0-9]+ [0-9]+\\.[0-9]+ [B|S]$"
