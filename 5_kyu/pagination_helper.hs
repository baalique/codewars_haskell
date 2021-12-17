-- https://www.codewars.com/kata/515bb423de843ea99400000a

module Codewars.Kata.Pagination where

type Collection a = [a]
type ItemsPerPage = Int

itemCount :: Collection a -> Int
itemCount = length

pageCount :: Collection a -> ItemsPerPage -> Int
pageCount xs n = ceiling $ fromIntegral (itemCount xs) / fromIntegral n

pageItemCount :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageItemCount xs n page | page < 0                   = Nothing
                        | page > pageCount xs n - 1  = Nothing
                        | page == pageCount xs n - 1 = Just $ (\c -> if c == 0 then n else c) (itemCount xs `mod` n)
                        | otherwise                  = Just n

pageIndex :: Collection a -> ItemsPerPage -> Int -> Maybe Int
pageIndex xs n item | itemCount xs == 0    = Nothing
                    | item >= itemCount xs = Nothing
                    | item < 0             = Just 0
                    | otherwise            = Just $ item `div` n
