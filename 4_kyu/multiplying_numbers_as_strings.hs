-- https://www.codewars.com/kata/55911ef14065454c75000062

module MultNumAsStrings where


multiply :: String -> String -> String
multiply xs ys = show $ read xs * read ys
