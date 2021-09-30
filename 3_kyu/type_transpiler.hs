-- https://www.codewars.com/kata/59a6949d398b5d6aec000007

module TypeTranspiler where

import           Data.List                      ( intercalate )
import           Text.Parsec                    ( (<|>)
                                                , char
                                                , choice
                                                , digit
                                                , eof
                                                , letter
                                                , many
                                                , oneOf
                                                , parse
                                                , sepBy
                                                , sepBy1
                                                , string
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

newtype Name = Name String

instance Show Name where
    show (Name "Int" ) = "Integer"
    show (Name "Unit") = "Void"
    show (Name n     ) = n

data TypeParam = StarParam
               | InTypeParam Type
               | OutTypeParam Type
               | TypeParam Type

instance Show TypeParam where
    show StarParam        = "?"
    show (InTypeParam  t) = "? super " ++ show t
    show (OutTypeParam t) = "? extends " ++ show t
    show (TypeParam    t) = show t

newtype TypeParams = TypeParams [TypeParam]

instance Show TypeParams where
    show (TypeParams tps) = intercalate "," (map show tps)

data SimpleUserType = SimpleUserType Name TypeParams

instance Show SimpleUserType where
    show (SimpleUserType n (TypeParams [])) = show n
    show (SimpleUserType n tps            ) = concat [show n, "<", show tps, ">"]

newtype UserType = UserType [SimpleUserType]

instance Show UserType where
    show (UserType ts) = intercalate "." (map show ts)

newtype Parameters = Parameters [Type]

instance Show Parameters where
    show (Parameters ts) = intercalate "," (map show ts)

data FunctionType = FunctionType Parameters Type

instance Show FunctionType where
    show (FunctionType ps@(Parameters []) t) = concat ["Function", "0", "<", show t, ">"]
    show (FunctionType ps@(Parameters ts) t) = concat ["Function", show $ length ts, "<", show ps, ",", show t, ">"]

data Type = TFunctionType FunctionType
          | TTypeName Name
          | TUserType UserType

instance Show Type where
    show (TFunctionType t) = show t
    show (TTypeName     t) = show t
    show (TUserType     t) = show t

transpile :: String -> Either String String
transpile input = case parse pTypeT "" input of
    Right t -> Right $ show t
    Left  _ -> Left "Hugh?"

pWhitespaces :: Parser String
pWhitespaces = many $ oneOf " \n\t"

pLexeme :: Parser a -> Parser a
pLexeme p = do
    pWhitespaces
    x <- p
    pWhitespaces
    return x

pName :: Parser Name
pName = do
    hd <- letter <|> char '_'
    tl <- many (letter <|> digit <|> char '_')
    let s = hd : tl
    return $ Name $ hd : tl

pTypeParam :: Parser TypeParam
pTypeParam =
    choice
        $   try
        .   pLexeme
        <$> [ pLexeme (char '*') >> return StarParam
            , pLexeme (string "in") >> (InTypeParam <$> pLexeme pType)
            , pLexeme (string "out") >> (OutTypeParam <$> pLexeme pType)
            , TypeParam <$> pLexeme pType
            ]

pTypeParams :: Parser TypeParams
pTypeParams = TypeParams <$> sepBy1 (pLexeme pTypeParam) (pLexeme $ char ',')

pSimpleUserType :: Parser SimpleUserType
pSimpleUserType =
    choice
        $   try
        .   pLexeme
        <$> [ do
                n <- pLexeme pName
                pLexeme $ char '<'
                tps <- pLexeme pTypeParams
                pLexeme $ char '>'
                return $ SimpleUserType n tps
            , (`SimpleUserType` TypeParams []) <$> pLexeme pName
            ]

pUserType :: Parser UserType
pUserType = UserType <$> sepBy1 (pLexeme pSimpleUserType) (pLexeme $ char '.')

pParameters :: Parser Parameters
pParameters = Parameters <$> sepBy (pLexeme pType) (pLexeme $ char ',')

pFunctionType :: Parser FunctionType
pFunctionType = do
    pLexeme $ char '('
    ps <- pLexeme pParameters
    pLexeme $ char ')'
    pLexeme $ string "->"
    FunctionType ps <$> pType

pType :: Parser Type
pType = do
    choice $ try . pLexeme <$> [TFunctionType <$> pFunctionType, TUserType <$> pUserType, TTypeName <$> pName]

pTypeT :: Parser Type
pTypeT = pType <* eof
