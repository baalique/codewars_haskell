module FindingAnAppointment where

import           Data.List                      ( (\\) )

getStartTime :: [[(String, String)]] -> Int -> Maybe String
getStartTime schedules duration = fmap convertFromTime (getStart duration $ getFreeTime schedules)

convertFromTime :: Int -> String
convertFromTime x = addZero (show (x `div` 60 + 9)) ++ ":" ++ addZero (show (x `mod` 60))
    where addZero s = if length s == 2 then s else '0' : s

convertTime :: String -> Int
convertTime [x1, x2, ':', x3, x4] = (read [x1, x2] - 9) * 60 + read [x3, x4]
convertTime _                     = undefined

convertInterval :: (String, String) -> [Int]
convertInterval (x1, x2) = [convertTime x1 .. convertTime x2 - 1]

convertTimetable :: [(String, String)] -> [Int]
convertTimetable = concatMap convertInterval

getFreeTime :: [[(String, String)]] -> [Int]
getFreeTime schedules = foldl (\\) [0 .. 599] (map convertTimetable schedules)

getStart :: Int -> [Int] -> Maybe Int
getStart d []           = Nothing
getStart d xs@(x : xs') = if take d xs == [x .. x + d - 1] then Just x else getStart d xs'
