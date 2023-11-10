import           Control.Applicative
import qualified Data.Map.Strict as M
import           Parser

-------------------------------------------------------------------------------

data Value
  = IntValue Int
  | FloatValue Float
  | StringValue String
  deriving (Eq, Show)

valueP :: Parser Value
valueP = IntValue <$> intP
  <|> FloatValue <$> floatP
  <|> StringValue <$> symbolsP

newtype Row = Row (M.Map String Value)
  deriving (Show)

rowP :: [String] -> Parser Row
rowP cNames = Row . M.fromList . zip cNames <$> sepBy (satisfyP (== ',')) valueP

data CSV = CSV {
    colNames :: [String] -- названия колонок в файле
  , rows     :: [Row]    -- список строк со значениями из файла
  } deriving (Show)

csvP :: Parser CSV
csvP = Parser f
  where
    f :: String -> Maybe (CSV, String)
    f s = case runParser colNamesP s of
        Nothing             -> Nothing
        Just (colNames, s') -> case runParser (rowsP colNames <|> pure []) s' of
            Nothing -> Nothing
            Just (rows, s'') -> Just (CSV colNames rows, s'')

    colNamesP :: Parser [String]
    colNamesP = sepBy (satisfyP (== ',')) symbolsP

    rowsP :: [String] -> Parser [Row]
    rowsP cNames = many (satisfyP (== '\n') *> rowP cNames)

-------------------------------------------------------------------------------

testIO :: IO (Maybe (CSV, String))
testIO = do
    content <- readFile "test.csv"
    return $ runParser csvP content
