-- https://www.codewars.com/kata/52b305bec65ea40fe90007a7

module Grabscrab where

import           Data.List                      ( sort )

grabScrab :: String -> [String] -> [String]
grabScrab msg = filter (\x -> sort x == sort msg)
