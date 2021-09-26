-- https://www.codewars.com/kata/5451712ea8a825a74f000863

module SimpleSQLEngine.Kata where

import           Data.Char                      ( toLower
                                                , toUpper
                                                )
import           Data.Function                  ( on )
import           Data.List                      ( elemIndex
                                                , intercalate
                                                , sortBy
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Map                      as M
import           Text.Parsec                    ( (<|>)
                                                , ParseError
                                                , char
                                                , choice
                                                , digit
                                                , eof
                                                , letter
                                                , many
                                                , many1
                                                , noneOf
                                                , parse
                                                , sepBy1
                                                , string
                                                , try
                                                )
import           Text.Parsec.String             ( Parser )

type Database = [(String, [[(String, String)]])]

type TableName = String
type ColumnName = String
type Const = String

type Comparison = [Ordering]

data ColumnID = ColumnID
  { getTableName  :: TableName
  , getColumnName :: ColumnName
  }
  deriving (Eq, Show)
data Value = ValueColumnID ColumnID | ValueConst Const deriving Show
data ValueTest = ValueTest Value Comparison Value
  deriving Show

data Query = Query
  { getSelect :: SelectClause
  , getFrom   :: FromClause
  , getJoins  :: [JoinClause]
  , getWhere  :: WhereClause
  }
  deriving Show

newtype SelectClause = SelectClause [ColumnID] deriving Show
newtype FromClause = FromClause TableName deriving Show
data JoinClause = JoinClause TableName ValueTest
  deriving Show
data WhereClause = WhereClause ValueTest | NoWhereClause deriving Show

type CellValue = String
data Cell = Cell
  { getColumnID :: ColumnID
  , getValue    :: CellValue
  }
  deriving Show
type Table = [[Cell]]
type DB = M.Map TableName Table

type QueryCellResult = (String, CellValue)
type QueryResult = [[QueryCellResult]]

sqlEngine :: Database -> String -> QueryResult
sqlEngine = execute

execute :: Database -> String -> QueryResult
execute database query =
  let db = convertDatabase database
  in  case parseQuery query of
        Right q -> convertQuery $ execQuery db q
        Left  _ -> undefined

execQuery :: DB -> Query -> Table
execQuery db q = execSelect db q $ execWhere db q $ execJoins db q $ execFrom db q

execFrom :: DB -> Query -> Table
execFrom db query = getTable tn db where FromClause tn = getFrom query

execJoins :: DB -> Query -> Table -> Table
execJoins db query table = foldl (flip $ execJoin db) table jcs where jcs = getJoins query

execWhere :: DB -> Query -> Table -> Table
execWhere db query table =
  let wc = getWhere query
  in  case wc of
        WhereClause vt -> filterByWhere vt table
        NoWhereClause  -> table

execSelect :: DB -> Query -> Table -> Table
execSelect db query table =
  let (SelectClause cis) = getSelect query
      sortRule           = compare `on` ((`elemIndex` cis) . getColumnID)
  in  map (sortBy sortRule . filter ((`elem` cis) . getColumnID)) table

execJoin :: DB -> JoinClause -> Table -> Table
execJoin db jc table = table >>= joinRows jt ci1 ci2
 where
  jt  = getTable tn db
  JoinClause tn (ValueTest (ValueColumnID ci1') _ (ValueColumnID ci2')) = jc
  ci1 = head $ filter ((== tn) . getTableName) [ci1', ci2']
  ci2 = head $ filter ((/= tn) . getTableName) [ci1', ci2']

joinRows :: Table -> ColumnID -> ColumnID -> [Cell] -> [[Cell]]
joinRows jt ci1 ci2 row =
  let cv = getCellValue row ci2
  in  case cv of
        Nothing  -> []
        Just cv' -> [ row ++ newRow | newRow <- filter (isJoining ci1 cv') jt ]

isJoining :: ColumnID -> CellValue -> [Cell] -> Bool
isJoining ci cv row =
  let cv' = getCellValue row ci
  in  case cv' of
        Nothing  -> False
        Just cv' -> cv' == cv

filterByWhere :: ValueTest -> Table -> Table
filterByWhere (ValueTest (ValueColumnID ci) cmp (ValueConst v)) = filter (filterRowByWhere ci cmp v)
filterByWhere _ = id

filterRowByWhere :: ColumnID -> Comparison -> Const -> [Cell] -> Bool
filterRowByWhere ci cmp v row =
  let cv' = getCellValue row ci
  in  case cv' of
        Just cv -> compare cv v `elem` cmp
        Nothing -> False

convertCell :: TableName -> (ColumnName, CellValue) -> Cell
convertCell tn (cn, val) = Cell (ColumnID tn cn) val

convertRow :: TableName -> [(ColumnName, CellValue)] -> [Cell]
convertRow = map . convertCell

convertTable :: (TableName, [[(ColumnName, CellValue)]]) -> (TableName, Table)
convertTable (tn, t) = (tn, map (convertRow tn) t)

convertDatabase :: [(TableName, [[(ColumnName, CellValue)]])] -> DB
convertDatabase = M.fromList . map convertTable

convertFromCell :: Cell -> QueryCellResult
convertFromCell (Cell (ColumnID tn cn) cv) = (concat [tn, ".", cn], cv)

convertQuery :: Table -> QueryResult
convertQuery = map (map convertFromCell)

getTable :: TableName -> DB -> Table
getTable tn db = db M.! tn

getCellValue :: [Cell] -> ColumnID -> Maybe CellValue
getCellValue row ci =
  let cs = filter (\(Cell ci' _) -> ci' == ci) row in if null cs then Nothing else (Just . getValue) $ head cs

escapeQuotes :: [Char] -> [Char]
escapeQuotes xs = intercalate "~" $ splitOn "''" xs

unescapeQuotes :: [Char] -> [Char]
unescapeQuotes = map (\x -> if x == '~' then '\'' else x)

parseQuery :: String -> Either ParseError Query
parseQuery = parse query "" . escapeQuotes

keyword :: String -> Parser String
keyword = mapM $ ((<|>) . char . toLower) <*> (char . toUpper)

whitespaces :: Parser String
whitespaces = many (char ' ' <|> char '\t' <|> char '\n')

validIdentifier :: Parser String
validIdentifier = many1 (letter <|> digit <|> char '_')

validConst :: Parser String
validConst = many1 (noneOf "\'")

tableName :: Parser TableName
tableName = validIdentifier

columnName :: Parser ColumnName
columnName = validIdentifier

valueConst :: Parser Value
valueConst = do
  char '\''
  c <- validConst
  char '\''
  return $ ValueConst $ unescapeQuotes c

columnID :: Parser ColumnID
columnID = do
  tn <- tableName
  char '.'
  ColumnID tn <$> columnName

valueColumnID :: Parser Value
valueColumnID = do
  tn <- tableName
  char '.'
  ValueColumnID . ColumnID tn <$> columnName

value :: Parser Value
value = choice [valueColumnID, valueConst]

comparison :: Parser Comparison
comparison = do
  v <- choice $ try . string <$> [">=", "<=", "<>", "=", ">", "<"]
  return $ case v of
    "="  -> [EQ]
    "<>" -> [LT, GT]
    ">"  -> [GT]
    "<"  -> [LT]
    ">=" -> [GT, EQ]
    "<=" -> [LT, EQ]
    _    -> []

valueTest :: Parser ValueTest
valueTest = do
  v1 <- value
  whitespaces
  c <- comparison
  whitespaces
  ValueTest v1 c <$> value

selectClause :: Parser SelectClause
selectClause = do
  keyword "select"
  whitespaces
  cis <- columnID `sepBy1` try (whitespaces <* char ',' <* whitespaces)
  return $ SelectClause cis

fromClause :: Parser FromClause
fromClause = do
  keyword "from"
  whitespaces
  FromClause <$> tableName

joinClause :: Parser JoinClause
joinClause = do
  keyword "join"
  whitespaces
  tn <- tableName
  whitespaces
  keyword "on"
  whitespaces
  vt <- valueTest
  whitespaces
  return $ JoinClause tn vt

whereClause :: Parser WhereClause
whereClause = do
  keyword "where"
  whitespaces
  WhereClause <$> valueTest

query :: Parser Query
query = do
  whitespaces
  s <- selectClause
  whitespaces
  f <- fromClause
  whitespaces
  js <- many joinClause
  whitespaces
  ws <- many whereClause
  let w = if null ws then NoWhereClause else head ws
  whitespaces
  eof
  return $ Query s f js w
