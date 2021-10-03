-- https://www.codewars.com/kata/598a82f07bad362e1d000003

module LispLovesMe where

import           Control.Monad                  ( guard )
import           Data.Functor                   ( ($>) )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import           Text.Parsec                    ( ParseError
                                                , between
                                                , char
                                                , choice
                                                , digit
                                                , many
                                                , many1
                                                , noneOf
                                                , oneOf
                                                , parse
                                                , string
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving Eq

instance Num AST where
    (+) (I32 v1) (I32 v2) = I32 (v1 + v2)
    (+) e1       e2       = reduce2 (+) e1 e2
    (*) (I32 v1) (I32 v2) = I32 (v1 * v2)
    (*) e1       e2       = reduce2 (*) e1 e2
    abs (I32 v) = I32 (abs v)
    abs e       = reduce1 abs e
    signum (I32 v) = I32 (signum v)
    signum e       = reduce1 signum e
    fromInteger i = I32 (fromInteger i)
    negate (I32 v) = I32 (-v)
    negate e       = reduce1 negate e

instance Ord AST where
    (<=) (I32 v1) (I32 v2) = v1 <= v2
    (<=) e1       e2       = False

instance Show AST where
    show (I32 v)     = show v
    show (Sym s)     = s
    show Nul         = "null"
    show Err         = "error"
    show (Lst es   ) = unwords (map show es)
    show (Boo True ) = "true"
    show (Boo False) = "false"
    show (Nod e [] ) = concat ["(", show e, ")"]
    show (Nod e es ) = concat ["(", show e, " "] ++ unwords (map show es) ++ ")"

lispEval :: String -> Maybe AST
lispEval = lispMaybe eval

lispPretty :: String -> Maybe String
lispPretty = lispMaybe show

lispMaybe :: (AST -> a) -> String -> Maybe a
lispMaybe f s = case parse pAST "" s of
    Right ast -> Just $ f ast
    Left  _   -> Nothing

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
    [ ("+"      , sum)
    , ("*"      , product)
    , ("-"      , foldl1 (-))
    , ("/"      , foldl1 fDiv)
    , ("^"      , foldl1 fPow)
    , (">"      , fCompare (>))
    , ("<"      , fCompare (<))
    , ("!"      , fNegate)
    , ("list"   , Lst)
    , ("size"   , fSize)
    , ("reverse", fReverse)
    , (".."     , fEnumFromThen)
    , ("=="     , fCompare (==))
    , (">="     , fCompare (>=))
    , ("<="     , fCompare (<=))
    , ("!="     , fCompare (/=))
    , ("if"     , fIf)
    ]

reduce1 :: (AST -> AST) -> AST -> AST
reduce1 f e = let e' = eval e in if e == e' then Err else f e'

reduce2 :: (AST -> AST -> AST) -> AST -> AST -> AST
reduce2 f e1 e2 =
    let e1' = eval e1
        e2' = eval e2
    in  if e1 == e1' && e2 == e2' then Err else f e1' e2'

reduceMap :: ([AST] -> AST) -> [AST] -> AST
reduceMap f es = let es' = map eval es in if es == es' then Err else f es'

fDiv :: AST -> AST -> AST
fDiv (I32 v1) (I32 v2) = I32 $ v1 `div` v2
fDiv e1       e2       = reduce2 fDiv e1 e2

fPow :: AST -> AST -> AST
fPow (I32 v1) (I32 v2) = I32 $ v1 ^ v2
fPow e1       e2       = reduce2 fPow e1 e2

fCompare :: (AST -> AST -> Bool) -> [AST] -> AST
fCompare c [e1@(I32 _), e2@(I32 _)] = toBoo $ c e1 e2
fCompare c [e1        , e2        ] = reduceMap (fCompare c) [e1, e2]
fCompare _ _                        = Err

fNegate :: [AST] -> AST
fNegate [Boo True ] = Boo False
fNegate [Boo False] = Boo True
fNegate [e        ] = reduceMap fNegate [e]
fNegate _           = Err

fSize :: [AST] -> AST
fSize [Lst es] = I32 $ length es
fSize [e     ] = reduceMap fSize [e]
fSize _        = Err

fReverse :: [AST] -> AST
fReverse [Lst es] = Lst (reverse es)
fReverse [e     ] = reduceMap fReverse [e]
fReverse _        = Err

fEnumFromThen :: [AST] -> AST
fEnumFromThen [I32 v1, I32 v2] = Lst $ I32 <$> [v1 .. v2]
fEnumFromThen [e1    , e2    ] = reduceMap fEnumFromThen [e1, e2]
fEnumFromThen _                = Err

fIf :: [AST] -> AST
fIf [pred, c1, c2] = case eval pred of
    Boo True  -> c1
    Boo False -> c2
    _         -> reduceMap fIf [pred, c1, c2]
fIf [pred, c] = case eval pred of
    Boo True  -> c
    Boo False -> Nul
    _         -> reduceMap fIf [pred, c]
fIf _ = Err

toBoo :: Bool -> AST
toBoo True  = Boo True
toBoo False = Boo False

eval :: AST -> AST
eval (Nod (Sym s) es) = if s `elem` map fst preludeFunctions then getFunction s es else Err
eval e                = e

getFunction :: String -> [AST] -> AST
getFunction = fromJust . flip M.lookup (M.fromList preludeFunctions)

toAST :: String -> Either ParseError AST
toAST = parse pAST ""

whitespaces :: String
whitespaces = ",\r\n\t "

pWhitespaces :: Parser String
pWhitespaces = many $ oneOf whitespaces

pLexeme :: Parser a -> Parser a
pLexeme = between pWhitespaces pWhitespaces

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

pAST :: Parser AST
pAST = choice $ try . pLexeme <$> [pI32, pSyn, pNul, pBoo, pNod]

pI32 :: Parser AST
pI32 = I32 . read <$> pLexeme (many1 digit)

pSyn :: Parser AST
pSyn = do
    hd <- noneOf $ " ,\n\t\r()" ++ ['0' .. '9']
    tl <- many $ noneOf " ,\n\t\r()"
    let s = hd : tl
    guard $ s `notElem` ["null", "true", "false"]
    return $ Sym s

pNul :: Parser AST
pNul =
    choice
        $   try
        .   pLexeme
        <$> [pLexeme (char '(') >> pLexeme (char ')') >> return Nul, pLexeme (string "null") >> return Nul]

pBoo :: Parser AST
pBoo = choice [pLexeme (string "true") $> Boo True, pLexeme (string "false") $> Boo False]

pNod :: Parser AST
pNod = parens (Nod <$> pLexeme pAST <*> many (pLexeme pAST))
