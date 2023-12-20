module HW7 where
import           Control.Monad (void)

import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import           Parser

-------------------------------------------------------------------------------

-- В этой домашке вам потребуется подгружать одновременно 2 файла в ghci:
-- src/Parser.hs и src/MyLib.hs. Это требует 2 шага:
--
-- ghci> :l src/Parser.hs src/MyLib.hs
-- ghci> :m Parser MyLib

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

-------------------------------------------------------------------------------

-- 2. Парсер FASTA (1,5 балла)

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

newLineP :: Parser Char
newLineP = satisfyP (== '\n')

-- :p, разобрался
fastaListP :: Parser [Fasta]
fastaListP = many $ Fasta <$ commentP <*> descriptP <* commentP <*> seqP <* commentP
    where
        commentP :: Parser ()
        commentP = void . many $ (satisfyP (==';') <* many (satisfyP (/='\n')) <* many newLineP)
        descriptP :: Parser String
        descriptP =    satisfyP (=='>')
                    *> many (satisfyP (/='\n'))
                    <* newLineP
        seqP :: Parser [Acid]
        seqP = mconcat <$> some (some (satisfyP (`elem` ('*' : ['A'..'Z']))) <* many newLineP)

-------------------
-------------------------------------------------------------------------------

-- 3. Парсер PDB (2 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL и ATOM

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = YourImplementationOfPDBAtom

-- | Тип, представляющий из себя MODEL
--
data PDBModel = YourImplementationOfPDBModel

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]

-- Распарсите `only_atoms.pdb`
-- Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
-- в которой может содержаться только секция ATOM.

-------------------------------------------------------------------------------
