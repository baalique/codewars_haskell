-- https://www.codewars.com/kata/595bbea8a930ac0b91000130

module Calculate1RM where

type Weight = Int
type Reps = Int

calculate1RM :: Weight -> Reps -> Weight
calculate1RM w 0 = 0
calculate1RM w 1 = w
calculate1RM w r = round $ maximum $ map (\f -> f (fromIntegral w) (fromIntegral r)) [f1, f2, f3]

f1 :: Double -> Double -> Double
f1 w r = w * (1 + r / 30)

f2 :: Double -> Double -> Double
f2 w r = 100 * w / (101.3 - 2.67123 * r)

f3 :: Double -> Double -> Double
f3 w r = w * (r ** 0.1)
