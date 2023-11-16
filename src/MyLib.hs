{-# LANGUAGE InstanceSigs #-}

module MyLib where

import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Char (isAlphaNum)
import           Data.Functor
import           Data.Maybe (isJust)
import           Parser

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
  deriving (Show, Eq)

-- | напишите парсер строки: заметьте, что теперь строка -- Map в `Maybe Value`
--
rowP :: [String] -> Parser Row
rowP cNames = Row . M.fromList . zip cNames <$> sepBy (satisfyP (== ',')) (spaceP *> (Just <$> valueP))

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

-- | В одном файле можно встретить несколько последовательностей, разделенных произвольным числом переводов строк
--   Напишите парсер контента такого файла (Пример в файле `test.fasta`)
--
fastaP :: Parser Fasta
fastaP = Fasta <$> descrP <*> seqP
  where
    commP :: Parser String
    commP = satisfyP (== ';') 
            *> many (satisfyP (/='\n'))

    descrP :: Parser String
    descrP = many commP
             *> satisfyP (== '>')
             *> some (satisfyP (/= '\n'))
    
    seqP :: Parser [Acid]
    seqP = many (satisfyP (\c -> c /= ';' && c /= '\n'))
           <* many commP
           <* many newLineP


fastaListP :: Parser [Fasta]
fastaListP = sepBy newLineP fastaP

-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
-- https://www.wwpdb.org/documentation/file-format-content/format33/sect9.html#MODEL

data PDBAtom = PDBAtom 
    { 
        serial_num :: Int
      , name       :: String
      , altLoc     :: Char
      , resName    :: String
      , chainID    :: Char
      , resSeq     :: Int
      , iCode      :: Char
      , x          :: Float
      , y          :: Float
      , z          :: Float
      , occupancy  :: Float
      , tempFactor :: Float
      , element    :: String
      , charge     :: String
    } 
    deriving (Show, Eq)

-- | Тип, представляющий из себя CONNECT
-- https://www.wwpdb.org/documentation/file-format-content/format33/sect10.html#CONECT
-- я так поняла, что одна связь должна быть, а  оставшиеся 3 не обязательны

data PDBBond = PDBBond 
    {
        atom_num  :: Int 
      , bondedA1  :: Int
      , bondedA2  :: Maybe Int
      , bondedA3  :: Maybe Int
      , bondedA4  :: Maybe Int
    }
    deriving (Eq, Show)

-- | Тип, представляющий из себя MODEL
--
data PDBModel
  = PDBModel
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      }

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.

line :: String -> Parser String
line = traverse (\c -> satisfyP (== c))

atomP :: Parser PDBAtom
atomP = PDBAtom
        <$> (line "ATOM" *> spaceP *> intP)
        <*> (spaceP *> symbolsP)
        <*> (oneSpaceP *> satisfyP (\c -> c == ' ' || isAlphaNum c))
        <*> (spaceP *> symbolsP)
        <*> (oneSpaceP *> satisfyP isAlphaNum)
        <*> (spaceP *> intP)
        <*> (satisfyP (== ' ') Data.Functor.$> ' ')
        <*> (spaceP *> floatP)
        <*> (spaceP *> floatP)
        <*> (spaceP *> floatP)
        <*> (spaceP *> floatP)
        <*> (spaceP *> floatP)
        <*> (spaceP *> symbolsP)
        <*> (spaceP *> symbolsP <* many (satisfyP (/= '\n')))

-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

connectP :: Parser PDBBond
connectP = PDBBond
           <$> (line "CONECT" *> spaceP *> intP) 
           <*> (spaceP *> intP)
           <*> optional (spaceP *> intP)
           <*> optional (spaceP *> intP)
           <*> optional (spaceP *> intP)

modelP :: Parser PDBModel
modelP = PDBModel
         <$> (line "MODEL" *> spaceP *> many atomP)
         <*> (spaceP *> many connectP) 
        
pdbP :: Parser PDB 
pdbP = PDB 
       <$> many modelP
-------------------------------------------------------------------------------

-- 4. Monad Parser (0,5 балла)
--    (можно без док-ва законов)

-- | Определен в файле Parser.hs
--   newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) = undefined

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
  fmap = undefined

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = undefined

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  (<*>) = undefined

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  (>>=) = undefined

---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)

-------------------------------------------------------------------------------
