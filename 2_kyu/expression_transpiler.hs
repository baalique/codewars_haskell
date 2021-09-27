-- https://www.codewars.com/kata/597ccf7613d879c4cb00000f

module Transpiler where

import           Data.List                      ( intercalate )
import           Text.Parsec                    ( (<|>)
                                                , char
                                                , choice
                                                , digit
                                                , eof
                                                , letter
                                                , many
                                                , many1
                                                , option
                                                , parse
                                                , sepBy
                                                , sepBy1
                                                , string
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

type Whitespaces = String
type Name = String
type Number = Int
data NameOrNumber = DataName Name
                  | DataNumber Number

newtype LambdaParam = LambdaParam NameOrNumber
newtype LambdaStatement = LambdaStatement NameOrNumber
data Lambda = Lambda [LambdaParam] [LambdaStatement]

data Expression = ExpressionNameOrNumber NameOrNumber
                | ExpressionLambda Lambda

newtype Parameter = Parameter Expression

data Function = FunctionWithParameters Expression [Parameter] [Lambda]
              | FunctionWithoutParameters Expression Lambda

instance Show NameOrNumber where
    show (DataName   n) = n
    show (DataNumber n) = show n

instance Show LambdaParam where
    show (LambdaParam n) = show n

instance Show LambdaStatement where
    show (LambdaStatement n) = show n

instance Show Lambda where
    show (Lambda ps sts) = ps' ++ sts'
      where
        ps'  = "(" ++ intercalate "," (map show ps) ++ ")"
        sts' = "{" ++ concatMap ((++ ";") . show) sts ++ "}"

instance Show Expression where
    show (ExpressionNameOrNumber n) = show n
    show (ExpressionLambda       l) = show l

instance Show Parameter where
    show (Parameter e) = show e

instance Show Function where
    show (FunctionWithParameters e ps ls) = show e ++ "(" ++ intercalate "," (map show ps ++ map show ls) ++ ")"
    show (FunctionWithoutParameters e l ) = show e ++ "(" ++ show l ++ ")"

transpile :: String -> Either String String
transpile str =
    let parsed = parse function "" str
    in  case parsed of
            Right s -> Right $ show s
            Left  _ -> Left "Hugh?"

whitespaces :: Parser Whitespaces
whitespaces = many (char ' ' <|> char '\t' <|> char '\n')

lexeme :: Parser a -> Parser a
lexeme p = do
    whitespaces
    x <- p
    whitespaces
    return x

dataName :: Parser NameOrNumber
dataName = do
    hd <- letter <|> char '_'
    tl <- many (letter <|> digit <|> char '_')
    return $ DataName $ hd : tl

dataNumber :: Parser NameOrNumber
dataNumber = do
    n <- many1 digit
    let m = read n
    return $ DataNumber m

nameOrNumber :: Parser NameOrNumber
nameOrNumber = dataName <|> dataNumber

lambdaStatement :: Parser LambdaStatement
lambdaStatement = LambdaStatement <$> nameOrNumber

lambdaParam :: Parser LambdaParam
lambdaParam = LambdaParam <$> nameOrNumber

lambdaParams :: Parser [LambdaParam]
lambdaParams = do
    params <- sepBy1 (lexeme lambdaParam) (char ',')
    lexeme $ string "->"
    return params

lambda :: Parser Lambda
lambda = do
    lexeme $ char '{'
    params     <- option [] (try lambdaParams)
    statements <- sepBy (lexeme lambdaStatement) whitespaces
    lexeme $ char '}'
    return $ Lambda params statements

expressionNameOrNumber :: Parser Expression
expressionNameOrNumber = ExpressionNameOrNumber <$> nameOrNumber

expressionLambda :: Parser Expression
expressionLambda = ExpressionLambda <$> lambda

expression :: Parser Expression
expression = choice [expressionNameOrNumber, expressionLambda]

parameter :: Parser Parameter
parameter = Parameter <$> expression

functionWithParameters :: Parser Function
functionWithParameters = do
    e <- lexeme expression
    lexeme $ char '('
    ps <- sepBy (lexeme parameter) (char ',')
    lexeme $ char ')'
    ls <- many (lexeme lambda)
    return $ FunctionWithParameters e ps ls

functionWithoutParameters :: Parser Function
functionWithoutParameters = do
    e <- lexeme expression
    l <- lexeme lambda
    return $ FunctionWithoutParameters e l

function :: Parser Function
function = choice (try <$> [functionWithParameters, functionWithoutParameters]) <* eof
