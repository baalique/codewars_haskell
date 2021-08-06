-- https://www.codewars.com/kata/525f47c79f2f25a4db000025

module Codewars.Kata.Phone where

import           Text.Regex.TDFA                ( (=~) )

validPhoneNumber :: String -> Bool
validPhoneNumber = (=~ "^\\([0-9]{3}\\) [0-9]{3}-[0-9]{4}$")
