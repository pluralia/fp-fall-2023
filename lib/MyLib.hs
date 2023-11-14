{-# LANGUAGE FlexibleInstances #-}
module MyLib where


import           Control.Applicative
import qualified Data.Map.Strict as M
import           Parser
import           Text.Megaparsec 
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Data.Void (Void)
import           Data.Char (isSpace)

-------------------------------------------------------------------------------

-- 1. Парсер строки CSV формата with Maybe (0,5 балла)

data Value
  = IntValue Int
  | FloatValue Float
  | StringValue String
  deriving (Eq, Show)

valueP :: Parser Value
valueP = IntValue <$> intP
  <|> FloatValue <$> floatP
  <|> StringValue <$> symbolsP

newtype Row = Row (M.Map String (Maybe Value))
  deriving (Show)

-- | напишите парсер строки: заметьте, что теперь строка -- Map в `Maybe Value`
--

-- Парсер для строки Row
rowP :: [String] -> Parser Row
rowP = undefined

-------------------------------------------------------------------------------

-- | Для чтения содержимого фалов в заданиях 2 и 3 используйте эту функцию
--
-- Эта функция testIO принимает путь к файлу и парсер Parsec Void String a. 
-- Она читает содержимое файла из указанного пути и затем парсит его с помощью предоставленного парсера. 
-- Результат будет либо Left с ошибкой парсинга, либо Right с успешно разобранными данными.

testParserIO :: FilePath -> Parsec Void String a -> IO Bool
testParserIO filePath parser = do
  result <- Text.Megaparsec.runParser parser "" <$> readFile filePath
  case result of
    Left _ -> return False    -- Парсер вернул ошибку
    Right _ -> return True    -- Парсер вернул успешный результат

-- чтобы проверить, какой результат выдает парсер прям в виде текста
-- testParserIO :: FilePath -> Parsec Void String [a] -> IO (Either String [a])
-- testParserIO filePath parser = do
--   result <- Text.Megaparsec.runParser parser "" <$> readFile filePath
--   case result of
--     Left err     -> return (Left (show err))  -- Парсер вернул ошибку
--     Right a      -> return (Right a)      -- Парсер вернул успешный результат


-- | Чтобы использовать файлы для тестов, воспользуйтесь этой функцией
--   Здесь мы просто проверяем, что результат парсинга не Nothing
-- 
-- testParserIO :: FilePath -> Parser a -> IO Bool
-- testParserIO filePath parser = isJust <$> testIO filePath parser

-- Вызывать `testParserIO` в тестах можно так
--     it "My test" $ do
--         testParserIO myFile myParser `shouldReturn` True

-- Другие тесты для задания 2 и 3 можно не писать

-- Парсер CSV для вдохновения можно найти в prac7.hs

-------------------------------------------------------------------------------

-- 2. Парсер FASTA (2,5 балла)

-- FASTA -- текстовый формат для нуклеотидных или полипептидных последовательностей,
-- в котором нуклеотиды или аминокислоты обозначаются при помощи однобуквенных кодов
-- (https://ru.wikipedia.org/wiki/FASTA)
-- Пример в файле `test.fasta`

-- | Для упрощения зададим нуклеотиды и аминокислоты одним и тем же типом Acid.
--   Для упрощения же будем считать, что это просто любой символ
--   (что не соответствует реальности -- взгляните на таблицы кодов для нуклеотидов и аминокислот) 
--
type Acid = Char

-- | Fasta состоит из 3 частей:
--   1. описание последовательности -- это одна строка любых символов, начинающаяся с `>` 
--   2. сама последовательность
--   3. (опционально) комментарий -- строка из любых символов, начинающаяся с `;` (необходимо игнорировать).
--      Договоримся, что комментарий может встречаться до/после описания/последовательности, и не разбивает
--      описание/последовательность на части
--
data Fasta = Fasta {
    description :: String  -- описание последовательности
  , seq         :: [Acid]  -- последовательность
} deriving (Eq, Show)

-- | В одном файле можно встретить несколько последовательностей, разделенных произвольным числом переводов строк
--   Напишите парсер контента такого файла (Пример в файле `test.fasta`)
--

fastaP :: Parsec Void String Fasta
fastaP = do
  -- _ <- skipMany (satisfy isSpace)  -- Пропуск пробелов
  _ <- char '>'
  desc <- someTill anySingle newline
  seqq <- Text.Megaparsec.some (satisfy (not . isSpace))
  return $ Fasta { description = desc, MyLib.seq = seqq }

fastaListP :: Parsec Void String [Fasta]
fastaListP = Text.Megaparsec.some fastaP

-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = PDBAtom
  { atomType :: String
  , atomNumber :: Int
  , atomName :: String
  , aminoAcid :: String
  , chainId :: Char
  , residueNumber :: Int
  , xCoord :: Double
  , yCoord :: Double
  , zCoord :: Double
  , occupancy :: Double
  , temperatureFactor :: Double
  , element :: String
  } deriving (Show)

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = PDBBond
  { atom1 :: Int
  , atom2 :: Int
  } deriving (Show)

-- | Тип, представляющий из себя MODEL
--
data PDBModel = PDBModel
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      } deriving (Show)

newtype PDBModel' = PDBModel'
  { aatoms :: [PDBAtom] 
  } deriving (Show)

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]

newtype PDB' = PDB' [PDBModel']

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.

-- Парсер для строки ATOM
atomParser :: Parsec Void String PDBAtom
atomParser = do
  aatomType <- string "ATOM" 
  aatomNumber <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.decimal
  aatomName <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.some alphaNumChar
  aaminoAcid <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.some alphaNumChar
  cchainId <- Text.Megaparsec.many Text.Megaparsec.Char.space *> anySingle
  rresidueNumber <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.decimal
  xxCoord <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.float
  yyCoord <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.float
  zzCoord <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.float
  ooccupancy <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.float
  ttemperatureFactor <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.float
  eelement <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.some (alphaNumChar <|> char '-' <|> char '+') <* eol
  return $ PDBAtom
    aatomType
    aatomNumber
    aatomName
    aaminoAcid
    cchainId
    rresidueNumber
    xxCoord
    yyCoord
    zzCoord
    ooccupancy
    ttemperatureFactor
    eelement

-- Парсер для секции MODEL, содержащей только ATOM
modelParser' :: Parsec Void String PDBModel'
modelParser' = do
  _ <- string "MODEL" 
  _ <- Text.Megaparsec.many Text.Megaparsec.Char.space *> (Text.Megaparsec.Char.Lexer.decimal :: Parsec Void String Int)  <* eol
  atomss <- Text.Megaparsec.some (Text.Megaparsec.many Text.Megaparsec.Char.space *> atomParser <* eol)
  _ <- string "ENDMDL"  <* eol
  return $ PDBModel' atomss

-- Парсер для всего файла PDB
pdbParser' :: Parsec Void String PDB'
pdbParser' = do
  models <- Text.Megaparsec.some modelParser'
  _ <- string "END" <* eol
  return $ PDB' models

-- Функция для тестирования парсера чтобы он выводил прямо в процессе работы результат
-- testAtomParser :: String -> IO ()
-- testAtomParser input = do
--   case Text.Megaparsec.runParser atomParser "" input of
--     Left err -> putStrLn $ "Error: " ++ errorBundlePretty err
--     Right result -> do
--       putStrLn "Parsed result:"
--       putStrLn $ "  atomType: " ++ atomType result
--       putStrLn $ "  atomNumber: " ++ show (atomNumber result)
--       putStrLn $ "  atomName: " ++ atomName result
--       putStrLn $ "  aminoAcid: " ++ aminoAcid result
--       putStrLn $ "  chainId: " ++ [chainId result]
--       putStrLn $ "  residueNumber: " ++ show (residueNumber result)
--       putStrLn $ "  xCoord: " ++ show (xCoord result)
--       putStrLn $ "  yCoord: " ++ show (yCoord result)
--       putStrLn $ "  zCoord: " ++ show (zCoord result)
--       putStrLn $ "  occupancy: " ++ show (occupancy result)
--       putStrLn $ "  temperatureFactor: " ++ show (temperatureFactor result)
--       putStrLn $ "  element: " ++ element result


-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

connectParser :: Parsec Void String PDBBond
connectParser= do
  _ <- string "CONNECT"
  bond1 <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.decimal
  bond2 <- Text.Megaparsec.many Text.Megaparsec.Char.space *> Text.Megaparsec.Char.Lexer.decimal <* eol
  return $ PDBBond bond1 bond2

-- Парсер для секции MODEL, содержащей ATOM и CONNECT
modelParser :: Parsec Void String PDBModel
modelParser = do
  _ <- string "MODEL" 
  _ <- Text.Megaparsec.many Text.Megaparsec.Char.space *> (Text.Megaparsec.Char.Lexer.decimal :: Parsec Void String Int)  <* eol
  atomss <- Text.Megaparsec.some (Text.Megaparsec.many Text.Megaparsec.Char.space *> atomParser <* eol)
  cconect <- Text.Megaparsec.many (Text.Megaparsec.Char.space *> connectParser <* eol)
  _ <- string "ENDMDL"  <* eol
  return $ PDBModel atomss cconect

-- Парсер для всего файла PDB где есть CONNECT
pdbParser :: Parsec Void String PDB
pdbParser = do
  models <- Text.Megaparsec.some modelParser
  _ <- string "END" <* eol
  return $ PDB models

-------------------------------------------------------------------------------

-- 4. Monad Parser (0,5 балла)
--    (можно без док-ва законов)

-- | Определен в файле Parser.hs
-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Реализация находится в файле Parser.hs

-------------------------------------------------------------------------------

-- 5. Реализуйте инстансы Applicative и Monad для нескольких типов (2,75 балла)

--    Покажите выполнение законов класса Applicative и Monad для вашей реализации (Functor не нужно)
--    https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html#t:Monad

---------------------------------------

-- 5.a Maybe (0,75 балла)

-- data Maybe' a = Nothing' | Just' a
--   deriving (Show, Eq)

-- Monad зависит от Applicative, Applicative -- от Functor,
-- поэтому нужно реализовывать и эти 2 класса при реализации Monad

-- instance Functor Maybe' where
--   fmap :: (a -> b) -> Maybe' a -> Maybe' b
--   fmap f (Just' x) = Just' (f x)
--   fmap _ Nothing'  = Nothing'

-- instance Applicative Maybe' where
--   pure :: a -> Maybe' a
--   pure = Just'

--   (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
--   Just' f <*> m = f <$> m
--   Nothing' <*> _ = Nothing'

-- instance Monad Maybe' where
--   (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
--   Just' x >>= f = f x
--   Nothing' >>= _ = Nothing'

---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

{-
Рассмотрим пример: если у нас есть fs :: Maybe' (a -> b) и xs :: Maybe' a, 
то кажется логичным применить каждую функцию из fs к соответствующему значению из xs. 
Таким образом, если у нас есть два списка значений, мы хотим применить первую функцию к первому значению, 
вторую к второму и так далее.

Для этого лучше использовать операцию zip. Операция zip берет два списка и объединяет их в список пар по позициям. 
Это идеально подходит для применения каждой функции к соответствующему значению, сохраняя структуру данных.
-}

---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

-- ??? додумать надо

-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)

{-
Стрелка Клейсли для монады m и типа a обозначается как a -> m b. 
Это функция, которая принимает значение типа a и возвращает вычисление в монаде m, результат которого имеет тип b.
-}

-------------------------------------------------------------------------------