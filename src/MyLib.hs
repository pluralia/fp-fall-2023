module MyLib where

import           Control.Applicative
import           Control.Monad (void)
import           Data.Char (isAlpha)
import           Data.Maybe (isJust)
import           Parser

-------------------------------------------------------------------------------

-- | Для чтения содержимого фалов в заданиях 2 и 3 используйте эту функцию
--
testIO :: FilePath -> Parser a -> IO (Maybe (a, String))
testIO filePath parser = do
    content <- readFile filePath         -- чтение из файла
    return $ runParser parser content    -- запуск парсера на содержимом

-- | Чтобы использовать файлы для тестов, воспользуйтесь этой функцией
--   Здесь мы просто проверяем, что результат парсинга не Nothing
-- 
testParserIO :: FilePath -> Parser a -> IO Bool
testParserIO filePath parser = isJust <$> testIO filePath parser

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

eofP :: Parser ()
eofP = Parser $ \s -> if null s then Just ((), s) else Nothing

notNewLine :: Char -> Bool
notNewLine = (/= '\n')

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP = many . satisfyP

lineStartsWithP :: Char -> Parser String
lineStartsWithP c = satisfyP (== c) *> takeWhileP (/= '\n')

-- | В одном файле можно встретить несколько последовательностей, разделенных произвольным числом переводов строк
--   Напишите парсер контента такого файла (Пример в файле `test.fasta`)
--
dropEmptyAndCommentsP :: Parser ()
dropEmptyAndCommentsP = void . many $ (pure <$> newLineP) <|> lineStartsWithP ';'

someSeqP :: Parser a -> Parser b -> Parser [b]
someSeqP sep singleElement = (:) <$> singleElement <*> many (sep *> singleElement)

fastaP :: Parser Fasta
fastaP = Fasta
  <$> lineStartsWithP '>' <* dropEmptyAndCommentsP
  <*> (concat <$> someSeqP newLineP (some (satisfyP (\c -> isAlpha c || c == '*'))))

fastaListP :: Parser [Fasta]
fastaListP = dropEmptyAndCommentsP *> sepBy dropEmptyAndCommentsP fastaP <* dropEmptyAndCommentsP <* eofP

-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
--
data PDBAtom
  = PDBAtom 
      { aserial :: Int
      , name :: String
      , altLoc :: Char
      , resName :: String
      , chainID :: Char
      , resSeq :: Int
      , iCode :: Char
      , x :: Float
      , y :: Float
      , z :: Float
      , occupancy :: Float
      , tempFactor :: Float
      , element :: String
      , charge :: String
      } deriving (Show, Eq)

stringP :: String -> Parser String
stringP = traverse (satisfyP . (==))

takeInLineP :: Int -> Parser String
takeInLineP 0 = pure []
takeInLineP c = (:) <$> satisfyP (/= '\n') <*> takeInLineP (c - 1)

-- Apply parser to output of another parser
infixl 6 >>>
(>>>) :: Parser String -> Parser a -> Parser a
(>>>) ps pa = Parser $ \s -> case runParser ps s of
  Nothing      -> Nothing
  Just (g, s') -> case runParser pa g of
    Just (a, "") -> Just (a, s')
    _            -> Nothing

spacedP :: Parser a -> Parser a
spacedP p = spaceP *> p <* spaceP

atomP :: Parser PDBAtom
atomP = PDBAtom
  <$  stringP "ATOM  "
  <*> takeInLineP 5 >>> spacedP intP
  <* oneSpaceP
  <*> takeInLineP 4
  <*> satisfyP notNewLine
  <*> takeInLineP 3
  <* oneSpaceP
  <*> satisfyP notNewLine
  <*> takeInLineP 4 >>> spacedP intP
  <*> satisfyP notNewLine
  <* stringP "   "
  <*> takeInLineP 8 >>> spacedP (floatP '.')
  <*> takeInLineP 8 >>> spacedP (floatP '.')
  <*> takeInLineP 8 >>> spacedP (floatP '.')
  <*> takeInLineP 6 >>> spacedP (floatP '.')
  <*> takeInLineP 6 >>> spacedP (floatP '.')
  <* stringP "          "
  <*> takeInLineP 2
  <*> takeInLineP 2

-- | Тип, представляющий из себя CONNECT
--
data PDBBond
  = PDBBond 
      { bserial1 :: Int
      , bserial2 :: Int
      , bserial3 :: Maybe Int
      , bserial4 :: Maybe Int
      , bserial5 :: Maybe Int
      } deriving (Show, Eq)

bondP :: Parser PDBBond
bondP = PDBBond
  <$  stringP "CONECT"
  <*> takeInLineP 5 >>> spacedP intP
  <*> takeInLineP 5 >>> spacedP intP
  <*> optional (takeInLineP 5 >>> spacedP intP)
  <*> optional (takeInLineP 5 >>> spacedP intP)
  <*> optional (takeInLineP 5 >>> spacedP intP)

-- | Тип, представляющий из себя MODEL
--
data PDBModel
  = PDBModel
      { mserial :: Int
      , atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      } deriving (Show, Eq)

modelP :: Parser PDBModel
modelP = PDBModel
  <$  stringP "MODEL "
  <*  stringP "    "
  <*> takeInLineP 4 >>> spacedP intP <* newLineP
  <*> many (atomP <* newLineP)
  <*> many (bondP <* newLineP)
  <*  stringP "ENDMDL"

-- | PDB-файл
--
newtype PDB = PDB [PDBModel] deriving (Show, Eq)

pdbP :: Parser PDB
pdbP = PDB
  <$> many (modelP <* newLineP)
  <*  stringP "END"
  <*  eofP

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.

-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.
