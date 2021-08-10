-- https://www.codewars.com/kata/54a2e93b22d236498400134b

module Haskell.Codewars.KeypadEntry where

import           Data.Char                      ( toUpper )
import           Data.Map                       ( (!)
                                                , Map
                                                , fromList
                                                )

presses :: String -> Int
presses = sum . map (getClicks . toUpper)

getClicks :: Char -> Int
getClicks c = read [keypad ! c] :: Int

keypad :: Map Char Char
keypad = fromList $ zip " ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890" "1123123123123123123412312341444445452"
