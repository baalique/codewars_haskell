-- https://www.codewars.com/kata/54c3ff09f3883230ac000689

module JpnTranslit where

translit :: String -> String
translit str = makeTranslit str []

makeTranslit :: String -> String -> String
makeTranslit []               acc = reverse acc
makeTranslit [x]              acc = makeTranslit [] (getPattern [x] ++ acc)
makeTranslit [x, y]           acc = makeTranslit rest (getPattern p ++ acc) 
    where (p, rest) = getMatch2 patternKeys [x, y]
makeTranslit (x : y : z : xs) acc = makeTranslit (rest ++ xs) (getPattern p ++ acc)
    where (p, rest) = getMatch3 patternKeys [x, y, z]

getMatch3 :: [String] -> String -> (String, String)
getMatch3 [] [x, y, z] = ([x], [y, z])
getMatch3 (p : ps) lst@[x, y, z] | p == lst    = (lst, "")
                                 | p == [x, y] = ([x, y], [z])
                                 | p == [x]    = ([x], [y, z])
                                 | otherwise   = getMatch3 ps lst
getMatch3 _ _ = undefined

getMatch2 :: [String] -> String -> (String, String)
getMatch2 [] [x, y] = ([x], [y])
getMatch2 (p : ps) lst@[x, y] | p == lst  = (lst, "")
                              | p == [x]  = ([x], [y])
                              | otherwise = getMatch2 ps lst
getMatch2 _ _ = undefined

patternKeys :: [String]
patternKeys = map fst patterns

getPattern :: String -> String
getPattern str = foldr (\(k, v) acc -> if k == str then v else acc) str patterns

firstMatch :: (t -> Bool) -> t -> [t] -> t
firstMatch _ d [] = d
firstMatch f d (x : xs) | f x       = x
                        | otherwise = firstMatch f d xs

patterns :: [(String, String)]
patterns =
    [ ("a"  , "あ")
    , ("i"  , "い")
    , ("u"  , "う")
    , ("e"  , "え")
    , ("o"  , "お")
    , ("ka" , "か")
    , ("ki" , "き")
    , ("ku" , "く")
    , ("ke" , "け")
    , ("ko" , "こ")
    , ("ga" , "が")
    , ("gi" , "ぎ")
    , ("gu" , "ぐ")
    , ("ge" , "げ")
    , ("go" , "ご")
    , ("sa" , "さ")
    , ("shi", "し")
    , ("su" , "す")
    , ("se" , "せ")
    , ("so" , "そ")
    , ("za" , "ざ")
    , ("ji" , "じ")
    , ("zu" , "ず")
    , ("ze" , "ぜ")
    , ("zo" , "ぞ")
    , ("ta" , "た")
    , ("chi", "ち")
    , ("tsu", "つ")
    , ("te" , "て")
    , ("to" , "と")
    , ("da" , "だ")
    , ("de" , "で")
    , ("do" , "ど")
    , ("na" , "な")
    , ("ni" , "に")
    , ("nu" , "ぬ")
    , ("ne" , "ね")
    , ("no" , "の")
    , ("ha" , "は")
    , ("hi" , "ひ")
    , ("fu" , "ふ")
    , ("he" , "へ")
    , ("ho" , "ほ")
    , ("ba" , "ば")
    , ("bi" , "び")
    , ("bu" , "ぶ")
    , ("be" , "べ")
    , ("bo" , "ぼ")
    , ("pa" , "ぱ")
    , ("pi" , "ぴ")
    , ("pu" , "ぷ")
    , ("pe" , "ぺ")
    , ("po" , "ぽ")
    , ("ma" , "ま")
    , ("mi" , "み")
    , ("mu" , "む")
    , ("me" , "め")
    , ("mo" , "も")
    , ("ya" , "や")
    , ("yu" , "ゆ")
    , ("yo" , "よ")
    , ("ra" , "ら")
    , ("ri" , "り")
    , ("ru" , "る")
    , ("re" , "れ")
    , ("ro" , "ろ")
    , ("wa" , "わ")
    , ("wo" , "を")
    , ("n"  , "ん")
    , ("m"  , "ん")
    , ("."  , "。")
    ]
