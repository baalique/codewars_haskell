-- https://www.codewars.com/kata/525c65e51bf619685c000059

module Baker where

type Ingredient = String
type Amount     = Int
type Recipe     = [(Ingredient, Amount)]
type Storage    = [(Ingredient, Amount)]

cakes :: Recipe -> Storage -> Int
cakes recipe storage = minimum ratios
  where
    ratios = map (\(ingr, amount) -> getIngredient ingr storage `div` amount) recipe
    getIngredient ingredient = foldl (\acc (ingr, amount) -> if ingr == ingredient then acc + amount else acc) 0
