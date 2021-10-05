-- https://www.codewars.com/kata/52dca71390c32d8fb900002b

module Codewars.Suffixes where

numberToOrdinal :: Int -> String
numberToOrdinal = reverse . appendSuffix . reverse . show
  where
    appendSuffix "0"             = "0"
    appendSuffix s@(_ : '1' : _) = "ht" ++ s
    appendSuffix s@('1'     : _) = "ts" ++ s
    appendSuffix s@('2'     : _) = "dn" ++ s
    appendSuffix s@('3'     : _) = "dr" ++ s
    appendSuffix s               = "ht" ++ s
