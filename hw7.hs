import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import           Parser
import qualified Text.Read.Lex as MyLib

-------------------------------------------------------------------------------

-- В этой домашке вам потребуется подгружать одновременно 2 файла в ghci:
-- src/Parser.hs и src/MyLib.hs. Это требует 2 шага:
--
-- ghci> :l src/Parser.hs src/MyLib.hs
-- ghci> :m Parser MyLib

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

-- | Hапишите парсер строки: заметьте, что теперь строка -- Map в `Maybe Value`
--   значения разделены запятыми; если значения между запятыми нет, используйте Nothing
--   x,y --> [Just x, Just y]
--   x,,y  --> [Just x, Nothing, Just y]
--
rowP :: [String] -> Parser Row
rowP cNames = undefined

-------------------------------------------------------------------------------

-- | Для чтения содержимого файлов в заданиях 2 и 3 используйте эту функцию
--
testIO :: FilePath -> Parser a -> IO (Maybe (a, String))
testIO filePath parser = do
    content <- readFile filePath         -- чтение из файла
    return $ runParser parser content    -- запуск парсера на содержимом

-- | Чтобы использовать содержимое файлы в тестах, воспользуйтесь этой функцией
--   Здесь мы просто проверяем, что парсер распарсил входную строку полностью
-- 
testFullyParsedIO :: FilePath -> Parser a -> IO Bool
testFullyParsedIO filePath parser = maybe False (null . snd) <$> testIO filePath parser

-- Вызывать `testFullyParsedIO` в тестах можно так
--     it "My test" $ do
--         testFullyParsedIO myFile myParser `shouldReturn` True

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
fastaListP :: Parser [Fasta]
fastaListP = undefined

-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = YourImplementationOfPDBAtom

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = YourImplementationOfPDBBond

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

---------------------------------------

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.

-- Важная подсказка:
-- вам понадобится парсер `floatP` -- обратите внимание, что `floatP` в Parser.hs, не умеет парсить отрицательные числа и
-- ожидает `,` в качестве разделителя вместо `.`

---------------------------------------

-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

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
