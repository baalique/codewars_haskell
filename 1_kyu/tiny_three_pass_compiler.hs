-- https://www.codewars.com/kata/5265b0885fda8eac5900093b

module TinyThreePassCompiler where

import           Data.Either                    ( fromRight )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import           Text.Parsec                    ( (<|>)
                                                , char
                                                , choice
                                                , digit
                                                , eof
                                                , letter
                                                , many
                                                , many1
                                                , oneOf
                                                , parse
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

type Whitespaces = String

newtype Number = Number Int deriving Show
data Operator = OAdd | OSub | OMul | ODiv deriving (Eq, Ord, Show)
newtype Identifier = Identifier String deriving (Eq, Show)

data Token = TNumber Number
           | TOperator Operator
           | TIdentifier Identifier
           | TOpenParen
           | TCloseParen
           deriving Show

data AsmToken = AImm Int
              | AArg Int
              | AAdd
              | ASub
              | AMul
              | ADiv
              deriving Show

data AsmInstruction = IM Int
                    | AR Int
                    | SW
                    | PU
                    | PO
                    | AD
                    | SU
                    | MU
                    | DI
                    deriving Show

type Expression = [Token]
type PostfixExpression = Expression

data Function = Function
    { getArgs :: [Identifier]
    , getBody :: Expression
    }
    deriving Show

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 s = postfixToAST args [] pf
  where
    f    = parseFunction s
    args = getArgs f
    body = getBody f
    pf   = toPostfix [] [] body

pass2 :: AST -> AST
pass2 ast = if isReducable ast then pass2 (reduceAST ast) else ast

pass3 :: AST -> [String]
pass3 = map show . postfixToAssembly [] . astToPostfix

postfixToAssembly :: [AsmInstruction] -> [AsmToken] -> [AsmInstruction]
postfixToAssembly stack []            = reverse $ PO : stack
postfixToAssembly stack (AImm n : ts) = postfixToAssembly (PU : IM n : stack) ts
postfixToAssembly stack (AArg n : ts) = postfixToAssembly (PU : AR n : stack) ts
postfixToAssembly stack (AAdd   : ts) = postfixToAssembly (PU : AD : PO : SW : PO : stack) ts
postfixToAssembly stack (ASub   : ts) = postfixToAssembly (PU : SU : PO : SW : PO : stack) ts
postfixToAssembly stack (AMul   : ts) = postfixToAssembly (PU : MU : PO : SW : PO : stack) ts
postfixToAssembly stack (ADiv   : ts) = postfixToAssembly (PU : DI : PO : SW : PO : stack) ts

reduceAST :: AST -> AST
reduceAST ast@(Imm _                ) = ast
reduceAST ast@(Arg _                ) = ast
reduceAST (    Add (Imm v1) (Imm v2)) = Imm (v1 + v2)
reduceAST (    Sub (Imm v1) (Imm v2)) = Imm (v1 - v2)
reduceAST (    Mul (Imm v1) (Imm v2)) = Imm (v1 * v2)
reduceAST (    Div (Imm v1) (Imm v2)) = Imm (v1 `div` v2)
reduceAST (    Add ast1     ast2    ) = Add (reduceAST ast1) (reduceAST ast2)
reduceAST (    Sub ast1     ast2    ) = Sub (reduceAST ast1) (reduceAST ast2)
reduceAST (    Mul ast1     ast2    ) = Mul (reduceAST ast1) (reduceAST ast2)
reduceAST (    Div ast1     ast2    ) = Div (reduceAST ast1) (reduceAST ast2)

isReducable :: AST -> Bool
isReducable (Imm _                ) = False
isReducable (Arg _                ) = False
isReducable (Add (Imm v1) (Imm v2)) = True
isReducable (Sub (Imm v1) (Imm v2)) = True
isReducable (Mul (Imm v1) (Imm v2)) = True
isReducable (Div (Imm v1) (Imm v2)) = True
isReducable (Add ast1     ast2    ) = isReducable ast1 || isReducable ast2
isReducable (Sub ast1     ast2    ) = isReducable ast1 || isReducable ast2
isReducable (Mul ast1     ast2    ) = isReducable ast1 || isReducable ast2
isReducable (Div ast1     ast2    ) = isReducable ast1 || isReducable ast2

postfixToAST :: [Identifier] -> [AST] -> PostfixExpression -> AST
postfixToAST args stack             []                              = head stack
postfixToAST args stack ((TIdentifier i) : ts) = postfixToAST args (Arg (getArgOrder args i) : stack) ts
postfixToAST args stack             ((TNumber     (Number n)) : ts) = postfixToAST args (Imm n : stack) ts
postfixToAST args (s1 : s2 : stack) (TOperator o              : ts) = postfixToAST args (getOperator o s2 s1 : stack) ts
postfixToAST _    _                 _                               = undefined

astToPostfix :: AST -> [AsmToken]
astToPostfix (Imm n        ) = [AImm n]
astToPostfix (Arg n        ) = [AArg n]
astToPostfix (Add ast1 ast2) = astToPostfix ast1 ++ astToPostfix ast2 ++ [AAdd]
astToPostfix (Sub ast1 ast2) = astToPostfix ast1 ++ astToPostfix ast2 ++ [ASub]
astToPostfix (Mul ast1 ast2) = astToPostfix ast1 ++ astToPostfix ast2 ++ [AMul]
astToPostfix (Div ast1 ast2) = astToPostfix ast1 ++ astToPostfix ast2 ++ [ADiv]

getOperator :: Operator -> AST -> AST -> AST
getOperator OAdd = Add
getOperator OSub = Sub
getOperator OMul = Mul
getOperator ODiv = Div

getArgOrder :: Eq a => [a] -> a -> Int
getArgOrder args arg = fromJust $ elemIndex arg args

toPostfix :: Expression -> Expression -> Expression -> PostfixExpression
toPostfix out stack [] = reverse out ++ stack
toPostfix out stack (t@(TIdentifier _) : ts) = toPostfix (t : out) stack ts
toPostfix out stack (t@(TNumber _) : ts) = toPostfix (t : out) stack ts
toPostfix out stack ((TOperator o) : ts) = toPostfix out' stack' ts where (out', stack') = handleOperator out stack o
toPostfix out stack (TOpenParen : ts) = toPostfix out (TOpenParen : stack) ts
toPostfix out stack (TCloseParen : ts) = toPostfix out' stack' ts where (out', stack') = handleCloseParen out stack

handleOperator :: Expression -> Expression -> Operator -> (Expression, Expression)
handleOperator out []       op = (out, [TOperator op])
handleOperator out (t : ts) op = case t of
    TOperator o | getPrecedence o >= getPrecedence op -> handleOperator (t : out) ts op
    _ -> (out, TOperator op : t : ts)

getPrecedence :: Operator -> Int
getPrecedence OAdd = 0
getPrecedence OSub = 0
getPrecedence _    = 1

handleCloseParen :: Expression -> Expression -> (Expression, Expression)
handleCloseParen out (TOpenParen : ts) = (out, ts)
handleCloseParen out (t          : ts) = handleCloseParen (t : out) ts
handleCloseParen _   _                 = undefined

pWhitespaces :: Parser Whitespaces
pWhitespaces = many $ oneOf " \n\t"

pLexeme :: Parser a -> Parser a
pLexeme p = do
    pWhitespaces
    x <- p
    pWhitespaces
    return x

pNumber :: Parser Number
pNumber = do
    n <- pLexeme (many1 digit)
    return $ Number $ read n

pOperator :: Parser Operator
pOperator = do
    o <- pLexeme $ oneOf "+-*/"
    case o of
        '+' -> return OAdd
        '-' -> return OSub
        '*' -> return OMul
        '/' -> return ODiv
        _   -> undefined

pIdentifier :: Parser Identifier
pIdentifier = do
    hd <- letter <|> char '_'
    tl <- many (letter <|> digit <|> char '_')
    let s = hd : tl
    return $ Identifier $ hd : tl

pToken :: Parser Token
pToken =
    choice
        $   try
        .   pLexeme
        <$> [ TNumber <$> pNumber
            , TOperator <$> pOperator
            , TIdentifier <$> pIdentifier
            , char '(' >> return TOpenParen
            , char ')' >> return TCloseParen
            ]

pExpression :: Parser Expression
pExpression = many $ pLexeme pToken

pFunction :: Parser Function
pFunction = do
    pLexeme $ char '['
    args <- many $ pLexeme pIdentifier
    pLexeme $ char ']'
    e <- pLexeme pExpression
    eof
    return $ Function { getArgs = args, getBody = e }

parseFunction :: String -> Function
parseFunction = fromRight (Function [] []) . parse pFunction ""
