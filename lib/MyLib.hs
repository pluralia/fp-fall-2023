{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module MyLib where


import           Control.Applicative
import qualified Data.Map.Strict as M
import           Parser
import           Text.Megaparsec 
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
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
rowP columns = Row . M.fromList . zip columns <$> Parser.sepBy (satisfyP (== '.')) (optional valueP)

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
-- testParserIO :: FilePath -> Parsec Void String a -> IO (Either String a)
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


-- main :: IO ()
-- main = do 
--   let testFilePath = "D:\\Studies_at_HSE_spb\\3 semestr\\HASKELL\\homeworks\\hw7\\smalltest.fasta"
  
--   result <- testParserIO testFilePath fastaP
--   case result of
--     Left err -> putStrLn $ "Parsing failed with error: " ++ err
--     Right parsedData -> putStrLn $ "Parsing successful. Parsed data: " ++ show parsedData
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
  aatomType <- string "ATOM" <* space1
  aatomNumber <- L.decimal <* space1
  aatomName <-  Text.Megaparsec.some alphaNumChar <* space1
  aaminoAcid <- Text.Megaparsec.some alphaNumChar <* space1
  cchainId <-  symbolChar <* space1
  rresidueNumber <- L.decimal <* space1
  xxCoord <- L.float <* space1
  yyCoord <- L.float <* space1
  zzCoord <- L.float <* space1
  ooccupancy <- L.float <* space1
  ttemperatureFactor <- L.float <* space1
  eelement <- Text.Megaparsec.some (alphaNumChar <|> symbolChar) <* newline
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
  _ <- string "MODEL"  <* Text.Megaparsec.Char.space *> (L.decimal :: Parsec Void String Int)  <* newline
  atomss <- Text.Megaparsec.many (atomParser <* newline) <* string "ENDMDL"  
  return $ PDBModel' atomss

-- Парсер для всего файла PDB
pdbParser' :: Parsec Void String PDB'
pdbParser' = do
  models <- Text.Megaparsec.many modelParser' <* string "END"
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
  bond1 <- Text.Megaparsec.Char.space *> L.decimal
  bond2 <- Text.Megaparsec.Char.space *> L.decimal <* eol
  return $ PDBBond bond1 bond2

-- Парсер для секции MODEL, содержащей ATOM и CONNECT
modelParser :: Parsec Void String PDBModel
modelParser = do
  _ <- string "MODEL" <* Text.Megaparsec.Char.space *> (L.decimal :: Parsec Void String Int)  <* eol
  atomss <- Text.Megaparsec.some (Text.Megaparsec.Char.space *> atomParser <* eol)
  cconect <- Text.Megaparsec.some (Text.Megaparsec.Char.space *> connectParser <* eol)
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

data Maybe' a = Nothing' | Just' a
  deriving (Show, Eq)

-- Monad зависит от Applicative, Applicative -- от Functor,
-- поэтому нужно реализовывать и эти 2 класса при реализации Monad

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap f (Just' x) = Just' (f x)
  fmap _ Nothing'  = Nothing'

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Just' f <*> m = f <$> m
  Nothing' <*> _ = Nothing'

-- -- Проверяем выполнение законов:
-- -- Проверка для Just' a
-- pure id <*> Just' a == Just' (id a) == Just' a

-- -- Проверка для Nothing'
-- pure id <*> Nothing' == Nothing'

-- -- Проверка для Just' f <*> pure y
-- Just' f <*> pure y == Just' (f y)

-- -- Проверка для pure ($ y) <*> Just' f
-- pure ($ y) <*> Just' f == Just' ($ y) <*> Just' f == Just' (f y)

-- -- Проверка для Nothing' <*> pure y
-- Nothing' <*> pure y == Nothing'

-- -- Проверка для pure ($ y) <*> Nothing'
-- pure ($ y) <*> Nothing' == Nothing'

-- -- Проверка для Just' (.)
-- pure (.) <*> Just' f <*> Just' g <*> Just' a == Just' (f . g) <*> Just' a == Just' (f (g a)) == Just' ((f . g) a)
-- Just' f <*> (Just' g <*> Just' a) == Just' f <*> Just' (g a) == Just' (f (g a)) == Just' ((f . g) a)

-- -- Проверка для Nothing' <*> Just' g <*> Just' a
-- Nothing' <*> Just' g <*> Just' a == Nothing'

-- -- Проверка для Just' f <*> Nothing' <*> Just' a
-- Just' f <*> Nothing' <*> Just' a == Nothing'

-- -- Проверка для Just' f <*> Just' g <*> Nothing'
-- Just' f <*> Just' g <*> Nothing' == Nothing'

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Just' x >>= f = f x
  Nothing' >>= _ = Nothing'

-- -- Проверяем выполнение законов:
-- leftIdentity a f = Just' a >>= f == f a
-- rightIdentity m = (m >>= Just') == m
-- associativity m f g = ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))


---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

{-
Рассмотрим на примере, пусть у нас есть список функций fs и список значений xs
fs = [(+1), (*2), (^2)]
xs = [1, 2, 3]

Если мы используем zip для применения каждой функции к каждому значению, результат будет выглядеть так:
zipWith ($) fs xs = [(+1) 1, (*2) 2, (^2) 3] = [2, 4, 9]

Однако, если мы используем оператор <*> для списков, результат будет:
fs <*> xs = [(+1) 1, (+1) 2, (+1) 3, (*2) 1, (*2) 2, (*2) 3, (^2) 1, (^2) 2, (^2) 3] = [2, 3, 4, 2, 4, 6, 1, 4, 9]

Таким образом, оператор <*> применяет каждую функцию из списка fs ко всем значениям списка xs, создавая новый список результатов. 
Это соответствует идее комбинирования каждого элемента первого списка с каждым элементом второго списка.

Я не совсем понял, это чисто теоретический вопрос или нужно прям реализовать код, поэтому
привел тут теорию, как я ее понял, без реализации <*> для списка
-}

---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

-- Если я правильно понял, то под "экстра" типом-параметром подразумевается следующее:
data Either' a b = Left' a | Right' b
  deriving (Show, Eq)

instance Functor (Either' a) where
    fmap :: (b -> c) -> Either' a b -> Either' a c
    fmap _ (Left' e) = Left' e   -- Если внутри Left, оставляем его неизменным
    fmap f (Right' r) = Right' (f r) -- Применяем функцию к значению внутри Right

-- Тогда реализация Applicative для Either' будет выглядеть следующим образом:
instance Applicative (Either' a) where
    pure :: b -> Either' a b
    pure = Right'
    
    (<*>) :: Either' a (c -> d) -> Either' a c -> Either' a d
    Left' e <*> _ = Left' e       -- Если первый аргумент - Left, оставляем его неизменным
    Right' f <*> r = fmap f r     -- Применяем функцию из Right к значению внутри Right

-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)

{-
Стрелка Клейсли для монады m и типа a обозначается как a -> m b. 
Это функция, которая принимает значение типа a и возвращает вычисление в монаде m, результат которого имеет тип b.
-}

-------------------------------------------------------------------------------
