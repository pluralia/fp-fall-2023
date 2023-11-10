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
rowP cNames = Row . M.fromList . zip cNames <$> listP valueP

data CSV = CSV {
    colNames :: [String] -- названия колонок в файле
  , rows     :: [Row]    -- список строк со значениями из файла
  } deriving (Show)

csvP :: Parser CSV
csvP = undefined
  where
    colNamesP :: Parser [String]
    colNamesP = listP symbolsP

-------------------------------------------------------------------------------

testIO :: IO (Maybe (CSV, String))
testIO = do
    content <- readFile "test.csv"
    return $ runParser csvP content
