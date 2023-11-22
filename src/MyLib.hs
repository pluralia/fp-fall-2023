{-# LANGUAGE InstanceSigs #-}

module MyLib where

import           Control.Applicative
import           Control.Monad (void)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (isJust)
import           Parser
import           Data.Char            (isAlpha)
import           Data.List            (singleton)

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

abstractRowP :: Char -> Parser a -> Parser [Maybe a]
abstractRowP sep p = moreElemP (optional p) <|> zeroElemP (optional p)
  where
    zeroElemP :: Parser (Maybe a) -> Parser [Maybe a]
    zeroElemP _ = [] <$ spaceP
    moreElemP :: Parser (Maybe a) -> Parser [Maybe a]
    moreElemP p' = (:)
      <$ spaceP
      <*> p'
      <*> many (spaceP
                *> satisfyP (==sep)
                *> spaceP
                *> p')

-- | напишите парсер строки: заметьте, что теперь строка -- Map в `Maybe Value`
--
rowP :: [String] -> Parser Row
rowP cNames = Row . M.fromList . zip cNames <$> abstractRowP ',' valueP

-------------------------------------------------------------------------------

-- | Для чтения содержимого файлов в заданиях 2 и 3 используйте эту функцию
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

-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = PDBAtom {serial      :: Int,         -- 7 - 11
                        name        :: String,      -- 13 - 16
                        altLoc      :: Char,        -- 17
                        resName     :: String,      -- 18 - 20
                        chainID     :: Char,        -- 22
                        resSeq      :: Int,         -- 23 - 26
                        iCode       :: Maybe Char,  -- 27
                        x           :: Float,       -- 31 - 38
                        y           :: Float,       -- 39 - 46
                        z           :: Float,       -- 47 - 54
                        occupancy   :: Float,       -- 55 - 60
                        tempFactor  :: Float,       -- 61 - 66
                        element     :: Char,        -- 77 - 78
                        charge      :: Maybe String -- 79 - 80
                        }
    deriving (Eq, Show)

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = PDBBond {number :: Int, -- 7 - 11
                        bond1  :: Maybe Int, -- 12 - 16
                        bond2  :: Maybe Int, -- 17 - 21
                        bond3  :: Maybe Int, -- 22 - 26
                        bond4  :: Maybe Int -- 27 - 31
                        }
    deriving (Eq, Show)

-- | Тип, представляющий из себя MODEL
--
data PDBModel
  = PDBModel
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      }
    deriving (Eq, Show)

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]
    deriving (Eq, Show)

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.
-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

atomP :: Parser PDBAtom
atomP = PDBAtom
        <$  spaceP
        <*> intP -- 11
        <*  spaceP -- 12 
        <*> (fourElemP <|> threeElemP <|> twoElemP <|> oneElemP) -- 13 - 16, буквы и цифры, возможно от 1 до 4 символов
        <*  spaceP
        <*> satisfyP isAlpha -- 17 Важно! Она есть всегда
        <*  spaceP
        <*> some (satisfyP isAlpha) -- 20
        <*  spaceP -- 21
        <*> satisfyP isAlpha -- 22
        <*  spaceP
        <*> intP -- 26
        <*> optional (satisfyP isAlpha) -- 27
        <*  spaceP
        <*> floatP' -- 38
        <*  spaceP
        <*> floatP' -- 46
        <*  spaceP
        <*> floatP' -- 54
        <*  spaceP
        <*> floatP' -- 60
        <*  spaceP
        <*> floatP' -- 66
        <*  spaceP
        <*> symbolP -- 78
        <*  spaceP
        <*> optional (some (satisfyP (`elem` ['+', '-'] <> ['1'..'9']))) -- 80
        <*  spaceP
  where
    oneElemP :: Parser String
    oneElemP = singleton <$> satisfyP isAlpha
    twoElemP :: Parser String
    twoElemP = (<>) <$> oneElemP <*> (singleton <$> symbolP)
    threeElemP :: Parser String
    threeElemP = (<>) <$> twoElemP <*> (singleton <$> symbolP)
    fourElemP :: Parser String
    fourElemP = (<>) <$> threeElemP <*> (singleton <$> symbolP)

bondP :: Parser PDBBond
bondP = PDBBond
        <$  spaceP
        <*> intP
        <*  spaceP
        <*> optional intP -- bond1
        <*  spaceP
        <*> optional intP -- bond2
        <*  spaceP
        <*> optional intP -- bond3
        <*  spaceP
        <*> optional intP -- bond4
        <*  spaceP

modelP :: Parser PDBModel
modelP = PDBModel
        <$  stringP "MODEL" <* many (satisfyP (/='\n')) <* newLineP -- парсер модели
        <*> many (stringP "ATOM  " *> atomP)                        -- атомов
        <*> (some (stringP "CONECT" *> bondP) <|> pure [])          -- связей
        <*  spaceP <* stringP "ENDMDL"                              -- конец модели

pdbP :: Parser PDB
pdbP = PDB <$> many modelP              -- парсер моделей
            <* spaceP <* stringP "END"  -- конец файла

-------------------------------------------------------------------------------

-- 4. Monad Parser (0,5 балла)
--    (можно без док-ва законов)

-- -- | Определен в файле Parser.hs
-- newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- инстанс определён в файле Parser.hs:

-- instance Monad Parser where
--     (>>=) :: Parser a -> (a -> Parser b) -> Parser b
--     (>>=) aP f = Parser bP
--       where
--         bP s = case runParser aP s of
--           Nothing      -> Nothing
--           Just (xx, s1) -> runParser (f xx) s1


-- | Доказательство законов:

-- 1. Left identity
-- return   :: a -> Parser a
-- return a :: Parser a
-- k        :: (a -> Parser b)

-- return a >>= k :: Parser a         -> (a -> Parser b)  -> Parser b
-- k a            :: (a -> Parser b)  -> a                -> Parser b
-- => return a >>= k = k a                                    

-- 2. Right identity
-- m      :: Parser ()
-- a      :: ()
-- return :: () -> Parser ()

-- Parser () >>= return :: Parser () -> (() -> Parser ()) -> Parser ()
-- => m >>= return = m

-- 3. Associativity
-- m          :: Parser a
-- k          :: (a -> Parser b)
-- k x        :: Parser b
-- h          :: (b -> Parser c)
-- k x >>= h  :: Parser c
-- m >>= k    :: Parser a -> (a -> Parser b) -> Parser b

-- m >>= (\x -> k x >>= h)  :: Parser a -> ((a -> Parser b) -> (b -> Parser c) -> Parser c) -> Parser c
-- (m >>= k) >>= h          :: (Parser a -> (a -> Parser b) -> Parser b) -> (b -> Parser c) -> Parser c
-- => m >>= (\x -> k x >>= h) = (m >>= k) >>= h

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
  fmap _ Nothing'  = Nothing'
  fmap f (Just' xx) = Just' (f xx)

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  (<*>) (Just' f) (Just' xx) = Just' (f xx)
  (<*>) _         _          = Nothing'

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  (>>=) (Just' xx) f = f xx
  (>>=) Nothing'   _ = Nothing'

---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

data List a = Null | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  (<>)  Null        list = list
  (<>) (Cons xx xs) list = Cons xx (xs <> list)

instance Monoid (List a) where
  mempty :: List a
  mempty = Null

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Null         = Null
  fmap f (Cons xx xs) = Cons (f xx) (fmap f xs)

instance Applicative List where
  pure :: a -> List a
  pure xx = Cons xx Null

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>)  Null         _           = Null
  (<*>)  _            Null        = Null
  (<*>) (Cons fx fs) (Cons xx xs) = Cons (fx xx) (fmap fx xs <> (fs <*> xs)) -- каждый с каждым

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=)  Null        _ = Null
  (>>=) (Cons xx xs) f = f xx <> (xs >>= f)

---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

data Either' a b = Left' a | Right' b
  deriving (Eq, Show)

instance Functor (Either' a) where
  fmap :: (b -> c) -> Either' a b -> Either' a c
  fmap _ (Left'  xx) = Left' xx
  fmap f (Right' xx) = Right' (f xx)

instance Applicative (Either' a) where
  pure :: b -> Either' a b
  pure = Right'

  (<*>) :: Either' a (b -> c) -> Either' a b -> Either' a c
  (<*>) (Left' xa)  _          = Left' xa
  (<*>)  _         (Left'  xa) = Left' xa
  (<*>) (Right' f) (Right' xx) = Right' (f xx)

instance Monad (Either' a) where
  (>>=) :: Either' a b -> (b -> Either' a c) -> Either' a c
  (>>=) (Left'  xa) _ = Left' xa
  (>>=) (Right' xx) f = f xx

-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)

-- Функция, принимающая некоторое значение и возвращающая "обёрнутое" в монад значение: 
-- k :: (a -> m b)

-- Простейший пример: return :: a -> m a
-- Эта функция ничего не изменяет, а просто "заворачивает" тип  a  в некоторый конструктор типов, 
-- для которого определён инстанс Monad.

-------------------------------------------------------------------------------
