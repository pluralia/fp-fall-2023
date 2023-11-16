{-# LANGUAGE InstanceSigs, FlexibleInstances, MultiParamTypeClasses #-}


module MyLib where

import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Char (isAlphaNum)
import           Data.Maybe (isJust)
import           Data.Functor
import           Parser

-------------------------------------------------------------------------------

-- 1. Парсер строки CSV формата with Maybe (0,5 балла)

data Value
  = IntValue Int
  | FloatValue Float
  | StringValue String
  deriving (Eq, Show)

valueP :: Parser Value
valueP = FloatValue  <$> floatP -- поменял месятами вещественные и целые числа, чтобы корректно отрабатывал rowP
     <|> IntValue    <$> intP
     <|> StringValue <$> symbolsP

newtype Row = Row (M.Map String (Maybe Value))
  deriving (Eq, Show)

-- | напишите парсер строки: заметьте, что теперь строка -- Map в `Maybe Value`
--
-- Реализация функции rowP
rowP :: [String] -> Parser Row
rowP colNames = Row <$> valuesMapP
  where
    valuesMapP = M.fromList . zip colNames <$> sepBy (satisfyP (== ',')) (spaceP *> (Just <$> valueP))

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
fastaP = Fasta
     <$> descriptionP
     <*> sequenceP
  where
    descriptionP :: Parser String
    descriptionP = many commentP
                *> satisfyP (== '>')
                *> some (satisfyP (/= '\n'))
                <* newLineP
    
    sequenceP :: Parser [Acid]
    sequenceP = many (satisfyP (\c -> c /= ';' && c /= '\n'))
               <* many commentP
               <* many newLineP

    commentP :: Parser String
    commentP = satisfyP (== ';')
            *> many (satisfyP (/= '\n'))
                
-- Парсер списка записей FASTA
fastaListP :: Parser [Fasta]
fastaListP = sepBy (many newLineP) fastaP


-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = PDBAtom
  { atomSerial     :: Int          -- Серийный номер атома
  , atomName       :: String       -- Имя атома
  , atomAltLoc     :: Char         -- Альтернативное расположение атома
  , atomResName    :: String       -- Имя остатка
  , atomChainID    :: Char         -- Идентификатор цепи
  , atomResSeq     :: Int          -- Номер остатка
  , atomICode      :: Char         -- Код вставки
  , atomX          :: Float        -- Координата X
  , atomY          :: Float        -- Координата Y
  , atomZ          :: Float        -- Координата Z
  , atomOccupancy  :: Float        -- Занимаемость места атомом
  , atomTempFactor :: Float        -- Температурный фактор
  , atomElement    :: String       -- Химический элемент атома
  , atomCharge     :: String       -- Заряд атома
  } deriving (Eq, Show)

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = PDBBond
    {
      atomOne     :: Int   -- номер первого атома
    , bondedAtoms :: [Int] -- номера атомов, связанных с ним
    }deriving (Eq, Show)

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
--
string :: String -> Parser String
string = traverse (\c -> satisfyP (== c))

-- | Парсер для секции ATOM
atomP :: Parser PDBAtom
atomP = PDBAtom
  <$> (string "ATOM" *> spaceP *> intP)                        -- atomSerial
  <*> (spaceP *> symbolsP)                                     -- atomName
  <*> (oneSpaceP *> satisfyP (\c -> c == ' ' || isAlphaNum c)) -- atomAltLoc
  <*> (spaceP *> symbolsP)                                     -- atomResName
  <*> (oneSpaceP *> satisfyP isAlphaNum)                       -- atomChainID
  <*> (spaceP *> intP)                                         -- atomResSeq
  <*> (satisfyP (== ' ') Data.Functor.$> ' ')                  -- atomICode - это hlint поправила
  <*> (spaceP *> floatP)                                       -- atomX
  <*> (spaceP *> floatP)                                       -- atomY
  <*> (spaceP *> floatP)                                       -- atomZ
  <*> (spaceP *> floatP)                                       -- atomOccupancy
  <*> (spaceP *> floatP)                                       -- atomTempFactor
  <*> (spaceP *> symbolsP)                                     -- atomElement
  <*> (spaceP *> symbolsP <* many (satisfyP (/= '\n')))        -- atomCharge 



-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

-- | Парсер для секции CONNECT
connectP :: Parser PDBBond
connectP =
  PDBBond
    <$> (string "CONECT" *> spaceP *> intP)        -- atom1
    <*> (spaceP *> sepBy spaceP intP)              -- bondedAtoms


-- | Парсер для секции MODEL
modelP :: Parser PDBModel
modelP =
  PDBModel
    <$> (string "MODEL" *> spaceP *> many atomP)  -- atoms
    <*> (spaceP *> many connectP)                 -- bonds

-- | Парсер для PDB-файла
pdbP :: Parser PDB
pdbP = PDB <$> many modelP
-------------------------------------------------------------------------------

-- 4. Monad Parser (0,5 балла)
--    (можно без док-ва законов)

-- | Определен в файле Parser.hs
--   newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- instance Monad Parser where
--     (>>=) :: Parser a -> (a -> Parser b) -> Parser b
--     pA >>= f = Parser $ \s -> case runParser pA s of
--         Nothing      -> Nothing
--         Just (a, s') -> runParser (f a) s'

-- убрал в Parser.hs, потому что ругается cabal test
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
  fmap f (Just' x) = Just' (f x)  

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  (<*>) Nothing'   _         = Nothing'
  (<*>) _          Nothing'  = Nothing'
  (<*>) (Just' f)  (Just' x) = Just' $ f x

-- Проверка законов Applicative для Maybe':
--
-- Закон 1 (Identity):
-- pure id <*> v = v
--
-- pure id <*> Nothing' = Nothing'       - Левая сторона
-- Nothing'             = Nothing'       - Правая сторона
--
-- pure id <*> (Just' x) = Just' (id x)  - Левая сторона
-- Just' x               = Just' x       - Правая сторона
--
-- Закон 2 (Composition):
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- pure (.) <*> Nothing' <*> Just' f <*> Just' x = Nothing'                   - Левая сторона
-- Nothing' <*> (Just' f <*> Just' x)            = Nothing'                   - Правая сторона
--
-- pure (.) <*> Just' g <*> Just' f <*> Just' x  = Just' (g . f) <*> Just' x  - Левая сторона
-- Just' (g . f) <*> Just' x                     = Just' (g (f x))            - Правая сторона
--
-- Закон 3 (Homomorphism):
-- pure f <*> pure x = pure (f x)
--
-- pure g <*> pure x = Just' (g x)  - Левая сторона
-- pure (g x)        = Just' (g x)  - Правая сторона
--
-- Закон 4 (Interchange):
-- u <*> pure y = pure ($ y) <*> u
--
-- Nothing' <*> pure y     = Nothing'   - Левая сторона
-- pure ($ y) <*> Nothing' = Nothing'   - Правая сторона
--
-- Just' f <*> pure y     = Just' (f y) - Левая сторона
-- pure ($ y) <*> Just' f = Just' (f y) - Правая сторона

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  (>>=) Nothing'   _ = Nothing'
  (>>=) (Just' x)  f = f x 

-- Проверка законов Monad для Maybe':
--
-- Закон 1 (Left Identity):
-- return a >>= f                 = f a
-- return a >>= \x -> Just' (f x) = Just' (f a)  - Левая сторона
-- Just' (f a)                    = Just' (f a)  - Правая сторона
--
-- Закон 2 (Right Identity):
-- m >>= return = m
-- Nothing' >>= return = Nothing'  - Левая сторона
-- Nothing'            = Nothing'  - Правая сторона
--
-- Закон 3 (Associativity):
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-- (Nothing' >>= f) >>= g                 = Nothing' - Левая сторона
-- Nothing'         >>= (\x -> f x >>= g) = Nothing' - Правая сторона
--
-- (Just' x >>= f) >>= g = (f x) >>= g        - Левая сторона
-- (f x)           >>= g = (f x) >>= g        - Правая сторона

---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil

  (<*>) :: List (a -> b) -> List a -> List b
  Nil <*> _           = Nil
  _ <*> Nil           = Nil
  (Cons f fs) <*> xs  = fmap f xs `append` (fs <*> xs)

-- Функция для объединения двух списков
append :: List a -> List a -> List a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (xs `append` ys)

-- Проверка законов Applicative для List:
-- Закон 1 (Identity):
--
-- pure id <*> v   = v
-- pure id <*> Nil = Nil  -- Левая сторона
-- Nil             = Nil  -- Правая сторона
-- pure id <*> Cons x xs = Cons (id x) (pure id <*> xs)  -- Левая сторона
-- Cons x xs             = Cons x xs                     -- Правая сторона
--
-- Закон 2 (Composition):
--
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure (.) <*> Nil <*> Cons f fs <*> Cons x xs = Nil  -- Левая сторона
-- Nil      <*> (Cons f fs <*> Cons x xs)       = Nil  -- Правая сторона
-- pure (.) <*> Cons g gs <*> Cons f fs <*> Cons x xs = Cons (g . f) (pure (.) <*> gs <*> fs <*> xs)  -- Левая сторона
-- Cons (g . f) (pure (.) <*> gs <*> fs <*> xs)       = Cons (g (f x)) (gs <*> (fs <*> xs))           -- Правая сторона
-- 
-- Закон 3 (Homomorphism):
-- pure f <*> pure x = pure (f x)
-- pure g <*> pure x = Cons (g x) Nil  -- Левая сторона
-- pure (g x)        = Cons (g x) Nil  -- Правая сторона
--
-- Закон 4 (Interchange):
-- u <*> pure y = pure ($ y) <*> u
-- Nil <*> pure y     = Nil  -- Левая сторона
-- pure ($ y) <*> Nil = Nil  -- Правая сторона
-- Cons f fs <*> pure y       = Cons (f y) (fs <*> pure y)  -- Левая сторона
-- Cons (f y) (fs <*> pure y) = Cons (f y) (fs <*> pure y)  -- Правая сторона

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  Nil >>= _           = Nil
  (Cons x xs) >>= f   = f x `append` (xs >>= f)

-- Проверка законов Monad для List:
-- Закон 1 (Left Identity):
-- return a >>= f = f a
-- return a >>= \x -> Cons (f x) Nil = Cons (f a) Nil  -- Левая сторона
-- Cons (f a) Nil                    = Cons (f a) Nil  -- Правая сторона

-- Закон 2 (Right Identity):
-- m >>= return = m
-- Nil >>= return = Nil  -- Левая сторона
-- Nil            = Nil  -- Правая сторона
-- Cons x xs >>= return = Cons x xs  -- Левая сторона
-- Cons x xs            = Cons x xs  -- Правая сторона

-- Закон 3 (Associativity):
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-- (Nil >>= f) >>= g         = Nil  -- Левая сторона
-- Nil >>= (\x -> f x >>= g) = Nil  -- Правая сторона
-- (Cons x xs >>= f) >>= g = (f x) >>= g  -- Левая сторона
-- (f x) >>= g = (f x)             >>= g  -- Правая сторона


---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

data Either' e a = Left' e | Right' a
  deriving (Show, Eq)

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap _ (Left' e)  = Left' e
  fmap f (Right' x) = Right' (f x)

instance Applicative (Either' e) where
  pure :: a -> Either' e a
  pure = Right'

  (<*>) :: Either' e (a -> b) -> Either' e a -> Either' e b
  Left' e <*> _            = Left' e
  _ <*> Left' e            = Left' e
  (Right' f) <*> something = fmap f something

-- Проверка законов Applicative для Either':
-- Закон 1 (Identity):
-- pure id <*> v = v
-- pure id <*> Left' e = Left' e  -- Левая сторона
-- Left' e             = Left' e  -- Правая сторона
-- pure id <*> Right' x = Right' (id x)  -- Левая сторона
-- Right' (id x)        = Right' (id x)  -- Правая сторона

-- Закон 2 (Composition):
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure (.) <*> Left' e1 <*> Left' e2 <*> Left' e3 = Left' e1  -- Левая сторона
-- Left' e1 <*> (Left' e2 <*> Left' e3)            = Left' e1  -- Правая сторона
-- pure (.) <*> Right' f <*> Right' g <*> Right' x = Right' (f . g x)  -- Левая сторона
-- Right' (f . g x)                                = Right' (f (g x))  -- Правая сторона

-- Закон 3 (Homomorphism):
-- pure f <*> pure x = pure (f x)
-- pure g <*> pure x = Right' (g x)  -- Левая сторона
-- Right' (g x)      = Right' (g x)  -- Правая сторона

-- Закон 4 (Interchange):
-- u <*> pure y = pure ($ y) <*> u
-- Left' e <*> pure y     = Left' e  -- Левая сторона
-- pure ($ y) <*> Left' e = Left' e  -- Правая сторона
-- Right' f <*> pure y = Right' (f y)  -- Левая сторона
-- Right' (f y)        = Right' (f y)  -- Правая сторона

instance Monad (Either' e) where
  (>>=) :: Either' e a -> (a -> Either' e b) -> Either' e b
  Left' e >>= _  = Left' e
  Right' x >>= f = f x

-- Проверка законов Monad для Either':
-- Закон 1 (Left Identity):
-- return a >>= f = f a
-- return a >>= \x -> Right' (f x) = Right' (f a)  -- Левая сторона
-- Right' (f a)                    = Right' (f a)  -- Правая сторона

-- Закон 2 (Right Identity):
-- m >>= return = m
-- Left' e >>= return = Left' e  -- Левая сторона
-- Left' e            = Left' e  -- Правая сторона
-- Right' x >>= return = Right' x  -- Левая сторона
-- Right' x            = Right' x  -- Правая сторона

-- Закон 3 (Associativity):
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-- (Left' e >>= f) >>= g         = Left' e  -- Левая сторона
-- Left' e >>= (\x -> f x >>= g) = Left' e  -- Правая сторона
-- (Right' x >>= f) >>= g = (f x) >>= g  -- Левая сторона
-- (f x)            >>= g = (f x) >>= g  -- Правая сторона


-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)

-- Пусть у нас есть монада M. Стрелка Клейсли для этой монады определяется следующим образом:
-- для любой функции f: A -> M B, где A и B - произвольные типы,
-- стрелка Клейсли Kleisli f представляет собой функцию, 
-- которая берет значение типа A и возвращает монаду M B.
-------------------------------------------------------------------------------