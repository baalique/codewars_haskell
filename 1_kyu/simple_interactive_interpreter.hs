-- https://www.codewars.com/kata/52ffcfa4aff455b3c2000750

module SimpleInteractiveInterpreter where

import           Control.Monad                  ( guard )
import           Data.List                      ( nub )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import           Text.Parsec                    ( (<|>)
                                                , ParseError
                                                , char
                                                , choice
                                                , digit
                                                , eof
                                                , letter
                                                , many
                                                , many1
                                                , oneOf
                                                , parse
                                                , string
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

data Interpreter = Interpreter
    { getFunctions :: Functions
    , getVariables :: Variables
    }
    deriving Eq

data InputRow = IExpression Expression
              | IFunction Function
              deriving Show

type Result = Maybe Double

type VariableName = Identifier
type FunctionName = Identifier
type VariableValue = Double

type Variables = M.Map VariableName Double
type Functions = M.Map FunctionName Function

type Whitespaces = String

data Operator = Eq | Add | Sub | Mul | Div | Mod | Const deriving (Eq, Ord)
newtype Number = Number Double deriving (Eq, Ord)
newtype Identifier = Identifier String deriving (Eq, Ord)

data SyntaxError = MismatchedParens
                 | InvalidOperator Operator
                 | InvalidToken Token
                 deriving Eq

data Error = InvalidInputError String
           | SyntaxError SyntaxError
           | InvalidExpressionError Expression
           | UnknownIdentifierError VariableName
           | VariableNotFoundInHeaderError VariableName
           | VariableNameConflictError VariableName
           | FunctionNameConflictError FunctionName
           | UnknownError Expression
           deriving Eq

type Expression = [Token]
type PostfixExpression = Expression

data Function = Function
    { functionName :: FunctionName
    , functionArgs :: [Identifier]
    , functionBody :: Expression
    }
    deriving (Eq, Show)

data Token = TOpenParen
            | TCloseParen
            | TNumber Number
            | TIdentifier Identifier
            | TOperator Operator
            deriving (Eq, Ord)

instance Show Interpreter where
    show (Interpreter fs vs) = concat ["Functions: ", show fs, "\nVariables: ", show vs]

instance Show Number where
    show (Number n) = show n

instance Show Identifier where
    show (Identifier i) = i

instance Show Operator where
    show Eq    = "="
    show Add   = "+"
    show Sub   = "-"
    show Mul   = "*"
    show Div   = "/"
    show Mod   = "%"
    show Const = "~"

instance Show Token where
    show TOpenParen      = "("
    show TCloseParen     = ")"
    show (TNumber     n) = show n
    show (TIdentifier i) = show i
    show (TOperator   o) = show o

instance Show SyntaxError where
    show MismatchedParens    = "SyntaxError: Mismatched parentheses"
    show (InvalidOperator o) = concat ["SyntaxError: Invalid operator '", show o, "'"]
    show (InvalidToken    t) = concat ["SyntaxError: Invalid token '", show t, "'"]

instance Show Error where
    show (InvalidInputError             s ) = concat ["InvalidInputError: '", s, "'"]
    show (SyntaxError                   se) = show se
    show (InvalidExpressionError        e ) = concat ["InvalidExpressionError: Invalid expression '", show e, "'"]
    show (UnknownIdentifierError        v ) = concat ["UnknownIdentifierError: Unknown identifier '", show v, "'"]
    show (VariableNotFoundInHeaderError v ) = concat
        [ "VariableNotFoundInHeaderError: Invalid identifier. No variable with name '"
        , show v
        , "' was found in function header"
        ]
    show (VariableNameConflictError v) =
        concat ["VariableNameConflictError: Function with the same name '", show v, "' already exists"]
    show (FunctionNameConflictError v) =
        concat ["FunctionNameConflictError: Variable with the same name '", show v, "' already exists"]
    show (UnknownError e) = concat ["UnknownError: Unknown error in expression '", show e, "'"]

newInterpreter :: Interpreter
newInterpreter = Interpreter M.empty M.empty

input :: String -> Interpreter -> Either String (Result, Interpreter)
input s i = case parseInterpreter s of
    Right r ->
        let ev = eval i r
        in  case ev of
                Right res -> Right res
                Left  err -> Left $ show err
    Left err -> Left $ show (InvalidInputError s)

parseInterpreter :: String -> Either ParseError InputRow
parseInterpreter = parse pInputRow ""

eval :: Interpreter -> InputRow -> Either Error (Result, Interpreter)
eval i@(Interpreter fs vs) (IFunction f) = do
    checkErrorsFunction i f
    convertExpressionToPostfix fs vs (functionBody f)
    return (Nothing, saveFunction i f)
eval i@(Interpreter fs vs) (IExpression e) = do
    checkErrorsExpression i e
    pe <- convertExpressionToPostfix fs vs e
    let as = getAssignmentsFromInfix e
    res <- evalExpression i as pe
    vs' <- getNewVariables fs vs as pe
    return (res, Interpreter fs (M.union vs' vs))

evalExpression :: Interpreter -> [Identifier] -> PostfixExpression -> Either Error Result
evalExpression (Interpreter fs vs) as e = evalPostfix fs vs as e []

checkErrorsFunction :: Interpreter -> Function -> Either Error ()
checkErrorsFunction (Interpreter fs vs) f@(Function fn is e) = getErrors checks
    where checks = [checkFunctionNameConflict vs fn, checkVariableNotFoundInHeader fs f]

checkErrorsExpression :: Interpreter -> Expression -> Either Error ()
checkErrorsExpression (Interpreter fs vs) e = getErrors checks
    where checks = [checkVariableNameConflict fs e, checkUnknownIdentifier fs vs e]

getMissingIdentifier :: [Identifier] -> [Identifier] -> Maybe Identifier
getMissingIdentifier is []       = Nothing
getMissingIdentifier is (v : vs) = if v `notElem` is then Just v else getMissingIdentifier is vs

getOddIdentifier :: [Identifier] -> [Identifier] -> Maybe Identifier
getOddIdentifier is []       = Nothing
getOddIdentifier is (v : vs) = if v `elem` is then Just v else getOddIdentifier is vs

checkFunctionNameConflict :: Variables -> FunctionName -> Maybe Error
checkFunctionNameConflict vs fn = FunctionNameConflictError <$> getOddIdentifier xs ys
  where
    xs = M.keys vs
    ys = [fn]

checkVariableNameConflict :: Functions -> Expression -> Maybe Error
checkVariableNameConflict fs e = VariableNameConflictError <$> getOddIdentifier xs ys
  where
    xs = getAssignmentsFromInfix e
    ys = M.keys fs

checkVariableNotFoundInHeader :: Functions -> Function -> Maybe Error
checkVariableNotFoundInHeader fs (Function fn is e) = VariableNotFoundInHeaderError <$> getMissingIdentifier xs ys
  where
    xs = is
    ys = getExtrasFromExpression (M.keys fs) e

checkUnknownIdentifier :: Functions -> Variables -> Expression -> Maybe Error
checkUnknownIdentifier fs vs e = UnknownIdentifierError <$> getMissingIdentifier xs ys
  where
    as = getAssignmentsFromInfix e
    xs = M.keys fs ++ M.keys vs ++ as
    ys = filterIdentifiers e

getErrors :: [Maybe Error] -> Either Error ()
getErrors checks | all isNothing checks = Right ()
                 | otherwise            = Left $ fromJust $ head $ filter isJust checks

saveFunction :: Interpreter -> Function -> Interpreter
saveFunction (Interpreter fs vs) f = Interpreter fs' vs where fs' = M.insert (functionName f) f fs

filterIdentifiers :: Expression -> [Identifier]
filterIdentifiers []                   = []
filterIdentifiers (TIdentifier i : ts) = i : filterIdentifiers ts
filterIdentifiers (t             : ts) = filterIdentifiers ts

getAssignmentsFromInfix :: Expression -> [Identifier]
getAssignmentsFromInfix [] = []
getAssignmentsFromInfix (TIdentifier i : TOperator Eq : ts) = i : getAssignmentsFromInfix ts
getAssignmentsFromInfix (t : ts) = getAssignmentsFromInfix ts

getNewVariables :: Functions -> Variables -> [Identifier] -> PostfixExpression -> Either Error Variables
getNewVariables fs vs as pe = saveVariablesPostfix fs vs as pe [] M.empty

getExtrasFromExpression :: [Identifier] -> Expression -> [Identifier]
getExtrasFromExpression is []       = []
getExtrasFromExpression is (TIdentifier i : ts) | i `notElem` is = i : getExtrasFromExpression is ts
getExtrasFromExpression is (t : ts) = getExtrasFromExpression is ts

convertExpressionToPostfix :: Functions -> Variables -> Expression -> Either Error PostfixExpression
convertExpressionToPostfix fs vs e = substituteFunctions fs vs e [] >>= toPostfix fs vs [] []

applyOperator :: Operator -> Double -> Double -> Double
applyOperator Eq    = const
applyOperator Add   = (+)
applyOperator Sub   = flip (-)
applyOperator Mul   = (*)
applyOperator Div   = flip (/)
applyOperator Mod   = \a b -> fromIntegral $ round b `mod` round a
applyOperator Const = const

evalPostfix :: Functions -> Variables -> [Identifier] -> PostfixExpression -> [Double] -> Either Error Result
evalPostfix fs vs as []                        []                 = Right Nothing
evalPostfix fs vs as []                        [v]                = Right $ Just v
evalPostfix fs vs as []                        st                 = Left $ InvalidExpressionError []
evalPostfix fs vs as (TNumber (Number n) : ts) st                 = evalPostfix fs vs as ts (n : st)
evalPostfix fs vs as (TIdentifier i : ts) st | i `elem` as        = evalPostfix fs vs as ts st
evalPostfix fs vs as pe@(TIdentifier i : ts) st | i `M.member` vs = case M.lookup i vs of
    Just v  -> evalPostfix fs vs as ts (v : st)
    Nothing -> Left $ InvalidExpressionError pe
evalPostfix fs vs as (TIdentifier i  : ts) st             = evalPostfix fs vs as ts st
evalPostfix fs vs as (TOperator   Eq : ts) st             = evalPostfix fs vs as ts st
evalPostfix fs vs as (TOperator   o  : ts) (v1 : v2 : st) = evalPostfix fs vs as ts (applyOperator o v1 v2 : st)
evalPostfix fs vs as ts                    st             = Left $ InvalidExpressionError ts

saveVariablesPostfix :: Functions
                     -> Variables
                     -> [Identifier]
                     -> PostfixExpression
                     -> PostfixExpression
                     -> Variables
                     -> Either Error Variables
saveVariablesPostfix fs vs as [] []  xs = Right xs
saveVariablesPostfix fs vs as [] [v] xs = Right xs
saveVariablesPostfix fs vs as [] ts  xs = Left $ InvalidExpressionError []
saveVariablesPostfix fs vs as (TNumber (Number n) : ts) st xs =
    saveVariablesPostfix fs vs as ts (TNumber (Number n) : st) xs
saveVariablesPostfix fs vs as (TIdentifier i : ts) st xs = if i `M.member` vs && i `notElem` as
    then saveVariablesPostfix fs vs as ts (TNumber (Number (vs M.! i)) : st) xs
    else saveVariablesPostfix fs vs as ts (TIdentifier i : st) xs
saveVariablesPostfix fs vs as (TOperator Eq : ts) (v1 : (TIdentifier i) : st) xs = case getNumberFromIdentifier v1 of
    Right v   -> saveVariablesPostfix fs vs as ts (v1 : st) (M.insert i v xs)
    Left  err -> Left err
saveVariablesPostfix fs vs as ts'@(TOperator Eq : ts) st             xs = Left $ InvalidExpressionError ts'
saveVariablesPostfix fs vs as (    TOperator o  : ts) (v1 : v2 : st) xs = do
    v1' <- getNumberFromIdentifier v1
    v2' <- getNumberFromIdentifier v2
    saveVariablesPostfix fs vs as ts (TNumber (Number $ applyOperator o v1' v2') : st) xs
saveVariablesPostfix fs vs as ts st xs = Left $ InvalidExpressionError ts

getNumberFromIdentifier :: Token -> Either Error Double
getNumberFromIdentifier i = case i of
    TNumber (Number n) -> Right n
    v                  -> Left $ SyntaxError $ InvalidToken v

toPostfix :: Functions -> Variables -> Expression -> Expression -> Expression -> Either Error PostfixExpression
toPostfix fs vs out stack []                           = Right $ reverse out ++ stack
toPostfix fs vs out stack (    t@(TNumber     n) : ts) = toPostfix fs vs (t : out) stack ts
toPostfix fs vs out stack ts'@(t@(TIdentifier i) : ts) = case i of
    i | i `M.member` fs -> Left $ UnknownError ts'
    i                   -> toPostfix fs vs (t : out) stack ts
toPostfix fs vs out stack (t@(TOperator op) : ts) = toPostfix fs vs out' stack' ts
    where (out', stack') = handleOperator out stack op
toPostfix fs vs out stack (TOpenParen  : ts) = toPostfix fs vs out (TOpenParen : stack) ts
toPostfix fs vs out stack (TCloseParen : ts) = case handleCloseParen out stack of
    Right (out', stack') -> toPostfix fs vs out' stack' ts
    Left  err            -> Left err

substituteFunctions :: Functions -> Variables -> Expression -> Expression -> Either Error Expression
substituteFunctions fs vs [] out = if hasFunctions fs vs out' then substituteFunctions fs vs out' [] else Right out'
    where out' = reverse out
substituteFunctions fs vs e@(TIdentifier i : ts) out | i `M.member` fs =
    let parsedArgs = getFunctionArgs fs vs i ts
    in  case parsedArgs of
            Right Nothing             -> Left $ UnknownError e
            Right (Just (args, rest)) -> case substituteFunction fs vs i args of
                Right f   -> substituteFunctions fs vs rest (reverse (encloseWithParens f) ++ out)
                Left  err -> Left err
            Left err -> Left err
substituteFunctions fs vs (t : ts) out = substituteFunctions fs vs ts (t : out)

substituteFunction :: Functions -> Variables -> Identifier -> [Expression] -> Either Error Expression
substituteFunction fs vs fn args = case fn `M.lookup` fs of
    Just f ->
        let body    = functionBody f
            argsMap = zip (TIdentifier <$> functionArgs f) args
        in  Right $ foldl (\acc (tn, as) -> replaceTokens tn as acc []) body argsMap
    Nothing -> Left $ UnknownIdentifierError fn

hasFunctions :: Functions -> Variables -> Expression -> Bool
hasFunctions fs vs []       = False
hasFunctions fs vs (TIdentifier i : ts) | i `M.member` fs = True
hasFunctions fs vs (_ : ts) = hasFunctions fs vs ts

encloseWithParens :: Expression -> Expression
encloseWithParens ts = [TOpenParen] ++ ts ++ [TCloseParen]

replaceTokens :: Token -> Expression -> Expression -> Expression -> Expression
replaceTokens tn rs [] out                 = reverse out
replaceTokens tn rs (t : ts) out | t == tn = replaceTokens tn rs ts (reverse (encloseWithParens rs) ++ out)
replaceTokens tn rs (t : ts) out           = replaceTokens tn rs ts (t : out)

getFunctionArgs :: Functions -> Variables -> Identifier -> Expression -> Either Error (Maybe ([Expression], Expression))
getFunctionArgs fs vs fn ts = do
    ar <- getArity fs fn
    getFunctionArgs' fs vs ar ts [] []

getFunctionArgs' :: Functions
                 -> Variables
                 -> Int
                 -> Expression
                 -> Expression
                 -> [Expression]
                 -> Either Error (Maybe ([Expression], Expression))
getFunctionArgs' fs vs 0 ts st acc = Right $ Just (reverse acc, ts)
getFunctionArgs' fs vs ar ts st acc | isValidExpression fs vs st == Right True =
    getFunctionArgs' fs vs (ar - 1) ts [] (st : acc)
getFunctionArgs' fs vs ar (t : ts) st acc = getFunctionArgs' fs vs ar ts (st ++ [t]) acc
getFunctionArgs' fs vs ar []       st acc = Right Nothing

getArity :: Functions -> Identifier -> Either Error Int
getArity fs fn = case fn `M.lookup` fs of
    Nothing -> Left $ UnknownIdentifierError fn
    Just f  -> Right $ length $ functionArgs f

isValidExpression :: Functions -> Variables -> Expression -> Either Error Bool
isValidExpression fs vs []                                     = Right False
isValidExpression fs vs [TNumber _]                            = Right True
isValidExpression fs vs [TIdentifier i] | i `M.member` vs      = Right True
isValidExpression fs vs [TIdentifier i] | i `M.member` fs      = Right False
isValidExpression fs vs (TIdentifier i : ts) | i `M.member` fs = do
    parsedArgs <- getFunctionArgs fs vs i ts
    return $ case parsedArgs of
        Nothing -> False
        Just (args, rest) ->
            let noTail       = null rest
                completeArgs = allRight (== True) (isValidExpression fs vs <$> args)
            in  noTail && completeArgs
isValidExpression fs vs (TNumber n : TOperator _ : ts')                       = isValidExpression fs vs ts'
isValidExpression fs vs (TNumber n : _) = Left $ SyntaxError (InvalidToken (TNumber n))
isValidExpression fs vs (TIdentifier i : TOperator _ : ts') | i `M.member` vs = isValidExpression fs vs ts'
isValidExpression fs vs (   TIdentifier i : TOperator Eq : ts')               = isValidExpression fs vs ts'
isValidExpression fs vs (TIdentifier i : _) = Left $ SyntaxError (InvalidToken (TIdentifier i))
isValidExpression fs vs [   TOpenParen                        ]               = Right False
isValidExpression fs vs ts@(TOpenParen : ts'                  )               = do
    b <- isValidExpression fs vs (init ts')
    return $ last ts == TCloseParen && b
isValidExpression fs vs (TCloseParen : _) = Left $ SyntaxError MismatchedParens
isValidExpression fs vs (TOperator o : _) = Left $ SyntaxError (InvalidOperator o)

allRight :: (t -> Bool) -> [Either a t] -> Bool
allRight f []             = True
allRight f (Right x : xs) = f x && allRight f xs
allRight f (Left  x : xs) = False

handleOperator :: Expression -> Expression -> Operator -> (Expression, Expression)
handleOperator out []       op = (out, [TOperator op])
handleOperator out (t : ts) op = case t of
    TOperator o | o >= op && op /= Eq -> handleOperator (t : out) ts op
    _ -> (out, TOperator op : t : ts)

handleCloseParen :: Expression -> Expression -> Either Error (Expression, Expression)
handleCloseParen out []                = Left $ SyntaxError MismatchedParens
handleCloseParen out (TOpenParen : ts) = Right (out, ts)
handleCloseParen out (t          : ts) = handleCloseParen (t : out) ts

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
    n <-
        choice
        $   try
        .   pLexeme
        <$> [ do
                d1 <- many1 digit
                char '.'
                d2 <- many1 digit
                return $ d1 ++ "." ++ d2
            , many1 digit
            ]
    return $ Number $ read n

pIdentifier :: Parser Identifier
pIdentifier = do
    hd <- letter <|> char '_'
    tl <- many (letter <|> digit <|> char '_')
    let s = hd : tl
    guard $ s /= "fn"
    return $ Identifier $ hd : tl

pOperator :: Parser Operator
pOperator = do
    o <- pLexeme $ oneOf "=+-*/%"
    case o of
        '+' -> return Add
        '-' -> return Sub
        '*' -> return Mul
        '/' -> return Div
        '%' -> return Mod
        '=' -> return Eq
        _   -> return Const

pTokens :: Parser Expression
pTokens = do
    cs <- many
        (   choice
        $   try
        .   pLexeme
        <$> [ char '(' >> return TOpenParen
            , char ')' >> return TCloseParen
            , TNumber <$> pLexeme pNumber
            , TOperator <$> pLexeme pOperator
            , TIdentifier <$> pLexeme pIdentifier
            ]
        )
    eof
    return cs

pExpression :: Parser Expression
pExpression = pTokens

pFunction :: Parser Function
pFunction = do
    pLexeme $ string "fn"
    fn   <- pLexeme pIdentifier
    args <- many $ pLexeme pIdentifier
    pLexeme $ string "=>"
    e <- pLexeme pExpression
    guard $ args == nub args
    return $ Function fn args e

pInputRow :: Parser InputRow
pInputRow = do
    r <- choice $ try . pLexeme <$> [IFunction <$> pFunction, IExpression <$> pExpression]
    eof
    return r

interpreterIteration :: Interpreter -> IO ()
interpreterIteration i = do
    putStr "> "
    s <- getLine
    let res = input s i
    case res of
        Right res'@(r, i') -> do
            putStrLn $ concat [show r, "\n", show i']
            interpreterIteration i'
        Left err -> do
            print err
            interpreterIteration i

main :: IO ()
main = do
    interpreterIteration newInterpreter
