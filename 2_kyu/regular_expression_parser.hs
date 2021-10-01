-- https://www.codewars.com/kata/5470c635304c127cad000f0d

module RegExpParser where

import           Text.Parsec                    ( between
                                                , char
                                                , choice
                                                , eof
                                                , noneOf
                                                , parse
                                                )
import           Text.Parsec.String             ( Parser )
import           Text.ParserCombinators.Parsec.Expr
                                                ( Assoc(AssocLeft, AssocNone)
                                                , Operator(Infix, Postfix)
                                                , buildExpressionParser
                                                )


data RegExp = Normal Char
            | Any
            | ZeroOrMore RegExp
            | Or RegExp RegExp
            | Str [RegExp]
            | Parens RegExp
            deriving (Show, Eq)

parseRegExp :: String -> Maybe RegExp
parseRegExp s = case parse (regExp <* eof) "" s of
  Right r -> Just $ unParen r
  Left  _ -> Nothing

regExp :: Parser RegExp
regExp = buildExpressionParser table term

term :: Parser RegExp
term = choice [Normal <$> noneOf "()*|.", Any <$ char '.', Parens <$> parens regExp]

table :: [[Operator Char st RegExp]]
table = [[Postfix (ZeroOrMore <$ char '*')], [Infix (return str) AssocLeft], [Infix (Or <$ char '|') AssocNone]]

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

unParen :: RegExp -> RegExp
unParen r@(Normal _)   = r
unParen Any            = Any
unParen (ZeroOrMore r) = ZeroOrMore $ unParen r
unParen (Or r1 r2    ) = Or (unParen r1) (unParen r2)
unParen (Str    [r]  ) = unParen r
unParen (Str    rs   ) = Str $ unParen <$> rs
unParen (Parens r    ) = unParen r

isParen :: RegExp -> Bool
isParen (Normal _)     = False
isParen Any            = False
isParen (ZeroOrMore r) = isParen r
isParen (Or r1 r2    ) = isParen r1 || isParen r2
isParen (Str    rs   ) = any isParen rs
isParen (Parens r    ) = True

str :: RegExp -> RegExp -> RegExp
str (Parens r1) r2          = Str [Str (toList r1), r2]
str r1          (Parens r2) = Str [r1, Str (toList r2)]
str r1          r2          = Str $ toList r1 ++ toList r2

toList :: RegExp -> [RegExp]
toList (Str rs) = rs
toList r        = [r]
