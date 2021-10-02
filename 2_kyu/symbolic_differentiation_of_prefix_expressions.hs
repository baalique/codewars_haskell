-- https://www.codewars.com/kata/584daf7215ac503d5a0001ae

module SymbolicDifferentiationOfPrefixExpressions where

import           Control.Monad                  ( guard )
import           Data.Either                    ( fromRight )
import           Text.Parsec                    ( between
                                                , char
                                                , choice
                                                , digit
                                                , letter
                                                , many
                                                , many1
                                                , oneOf
                                                , parse
                                                , string
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

data Expression = Number Double
                | Variable String
                | Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Pow Expression Expression
                | Sin Expression
                | Cos Expression
                | Tan Expression
                | Exp Expression
                | Ln Expression
                deriving (Eq)

instance Show Expression where
    show (Number   n) = if fromIntegral r == n then show r else show n where r = round n
    show (Variable v) = v
    show (Add e1 e2 ) = concat ["(+ ", show e1, " ", show e2, ")"]
    show (Sub e1 e2 ) = concat ["(- ", show e1, " ", show e2, ")"]
    show (Mul e1 e2 ) = concat ["(* ", show e1, " ", show e2, ")"]
    show (Div e1 e2 ) = concat ["(/ ", show e1, " ", show e2, ")"]
    show (Pow e1 e2 ) = concat ["(^ ", show e1, " ", show e2, ")"]
    show (Sin e     ) = concat ["(sin ", show e, ")"]
    show (Cos e     ) = concat ["(cos ", show e, ")"]
    show (Tan e     ) = concat ["(tan ", show e, ")"]
    show (Exp e     ) = concat ["(exp ", show e, ")"]
    show (Ln  e     ) = concat ["(ln ", show e, ")"]

diff :: String -> String
diff = show . simplify . derivative . simplify . fromRight (Variable "") . parse pExpression ""

derivative :: Expression -> Expression
derivative (Number   _                 ) = Number 0
derivative (Variable _                 ) = Number 1
derivative (Add e1           e2        ) = Add (derivative e1) (derivative e2)
derivative (Sub e1           e2        ) = Sub (derivative e1) (derivative e2)
derivative (Mul e1           e2        ) = Add (Mul (derivative e1) e2) (Mul e1 (derivative e2))
derivative (Div e1 e2) = Div (Sub (Mul (derivative e1) e2) (Mul e1 (derivative e2))) (Pow e2 (Number 2))
derivative (Pow (Variable v) (Number n)) = Mul (Number n) (Pow (Variable v) (Number (n - 1)))
derivative (Pow e1           e2        ) = undefined
derivative (Sin e                      ) = Mul (derivative e) (Cos e)
derivative (Cos e                      ) = Mul (derivative e) (Mul (Number (-1)) (Sin e))
derivative (Tan e                      ) = Mul (derivative e) (Add (Number 1) (Pow (Tan e) (Number 2)))
derivative (Exp e                      ) = Mul (derivative e) (Exp e)
derivative (Ln  e                      ) = Mul (derivative e) (Div (Number 1) e)

simplify :: Expression -> Expression
simplify e = if e == e' then e else simplify e' where e' = reduce e

reduce :: Expression -> Expression
reduce e@(Number   _                         ) = e
reduce e@(Variable _                         ) = e
reduce (  Add (Number n1) (Number n2)        ) = Number (n1 + n2)
reduce (  Sub (Number n1) (Number n2)        ) = Number (n1 - n2)
reduce (  Mul (Number n1) (Number n2)        ) = Number (n1 * n2)
reduce (  Div (Number n1) (Number n2)        ) = Number (n1 / n2)
reduce (  Pow (Number n1) (Number n2)        ) = Number (n1 ** n2)
reduce (  Add (Number 0 ) e                  ) = reduce e
reduce (  Add e           (Number 0)         ) = reduce e
reduce (  Sub (Number 0)  e                  ) = Mul (Number (-1)) (reduce e)
reduce (  Sub e           (Number 0)         ) = reduce e
reduce (  Mul (Number 0)  e                  ) = Number 0
reduce (  Mul e           (Number 0)         ) = Number 0
reduce (  Mul (Number 1)  e                  ) = reduce e
reduce (  Mul e           (Number 1         )) = reduce e
reduce (  Mul (Number n1) (Mul (Number n2) e)) = Mul (Number (n1 * n2)) (reduce e)
reduce (  Mul (Number n1) (Mul e (Number n2))) = Mul (Number (n1 * n2)) (reduce e)
reduce (  Mul (Mul (Number n1) e) (Number n2)) = Mul (reduce e) (Number (n1 * n2))
reduce (  Mul (Mul e (Number n1)) (Number n2)) = Mul (reduce e) (Number (n1 * n2))
reduce (  Div (Number 0 ) e                  ) = Number 0
reduce (  Div e           (Number 1)         ) = reduce e
reduce (  Pow e           (Number 0)         ) = Number 1
reduce (  Pow e           (Number 1)         ) = reduce e
reduce (  Add e1          e2                 ) = Add (reduce e1) (reduce e2)
reduce (  Sub e1          e2                 ) = Sub (reduce e1) (reduce e2)
reduce (  Mul e1          e2                 ) = Mul (reduce e1) (reduce e2)
reduce (  Div e1          e2                 ) = Div (reduce e1) (reduce e2)
reduce (  Pow e1          e2                 ) = Pow (reduce e1) (reduce e2)
reduce (  Sin e                              ) = Sin (reduce e)
reduce (  Cos e                              ) = Cos (reduce e)
reduce (  Tan e                              ) = Tan (reduce e)
reduce (  Exp e                              ) = Exp (reduce e)
reduce (  Ln  e                              ) = Ln (reduce e)

pWhitespaces :: Parser String
pWhitespaces = many $ oneOf " \n\t"

pLexeme :: Parser a -> Parser a
pLexeme = between pWhitespaces pWhitespaces

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

pExpression :: Parser Expression
pExpression = choice $ try . pLexeme <$> [parens pSimpleExpression, pSimpleExpression]

pSimpleExpression :: Parser Expression
pSimpleExpression =
    choice
        $   try
        .   pLexeme
        <$> [ pNumber
            , pVariable
            , pOperator "+" Add
            , pOperator "-" Sub
            , pOperator "*" Mul
            , pOperator "/" Div
            , pOperator "^" Pow
            , pFunction "sin" Sin
            , pFunction "cos" Cos
            , pFunction "tan" Tan
            , pFunction "exp" Exp
            , pFunction "ln"  Ln
            ]

pNumber :: Parser Expression
pNumber = Number . read <$> many1 digit

pVariable :: Parser Expression
pVariable = do
    s <- many1 letter
    guard $ s `notElem` ["sin", "cos", "tan", "exp", "ln"]
    return $ Variable s

pOperator :: String -> (Expression -> Expression -> Expression) -> Parser Expression
pOperator s f = do
    pLexeme $ string s
    e1 <- pLexeme pExpression
    e2 <- pLexeme pExpression
    return $ f e1 e2

pFunction :: String -> (Expression -> Expression) -> Parser Expression
pFunction s f = do
    pLexeme $ string s
    e <- pLexeme pExpression
    return $ f e
