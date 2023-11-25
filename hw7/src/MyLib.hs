{-# LANGUAGE InstanceSigs, FlexibleInstances, MultiParamTypeClasses #-}


module MyLib where

import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Char (isAlpha)
import           Data.Maybe (isJust)
import           Data.Functor ()
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


testFullyParsedIO :: FilePath -> Parser a -> IO Bool
testFullyParsedIO filePath parser = maybe False (null . snd) <$> testIO filePath parser

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
     <$> (many commentP *> descriptionP)
     <*> (many commentP *> sequencesP)
     <*   many commentP
  where
    descriptionP :: Parser String
    descriptionP = satisfyP (== '>')
                *> many (satisfyP (/= '\n'))
    
    sequencesP :: Parser [Acid]
    sequencesP = concat
              <$> many (satisfyP (\c -> c `notElem` [';', '>'])
               *> many (satisfyP (/= '\n'))
              <* many newLineP)

    commentP :: Parser String
    commentP = satisfyP (== ';')
            *> many (satisfyP (/= '\n'))
            <* many newLineP
                
-- Парсер списка записей FASTA
fastaListP :: Parser [Fasta]
fastaListP = many fastaP


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
  , atomICode      :: Maybe Char   -- Код вставки
  , atomX          :: Float        -- Координата X
  , atomY          :: Float        -- Координата Y
  , atomZ          :: Float        -- Координата Z
  , atomOccupancy  :: Float        -- Занимаемость места атомом
  , atomTempFactor :: Float        -- Температурный фактор
  , atomElement    :: Char         -- Химический элемент атома
  , atomCharge     :: Maybe String -- Заряд атома
  } deriving (Eq, Show)

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = PDBBond
  {number :: Int,
   bond1  :: Maybe Int,
   bond2  :: Maybe Int
    } deriving (Eq, Show)


-- | Тип, представляющий из себя MODEL
--
data PDBModel
  = PDBModel
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      } deriving (Eq, Show)

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]
  deriving (Eq, Show)

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.


atomP :: Parser PDBAtom
atomP = PDBAtom
  <$  spaceP                                    -- Игнорируем пробелы перед началом парсинга
  <*> intP <* spaceP                             -- Парсим целое число (atomSerial) с пробелом после
  <*> symbolsP <* spaceP                         -- Парсим строку символов (atomName) с пробелом после
  <*> satisfyP isAlpha <* spaceP                 -- Парсим одну букву (atomAltLoc) с пробелом после
  <*> some (satisfyP isAlpha) <* spaceP          -- Парсим одну или более букв (atomResName) с пробелом после
  <*> satisfyP isAlpha <* spaceP                 -- Парсим одну букву (atomChainID) с пробелом после
  <*> intP <* spaceP                             -- Парсим целое число (atomResSeq) с пробелом после
  <*> optional (satisfyP isAlpha) <* spaceP      -- Парсим опциональную букву (atomICode) с пробелом после
  <*> floatWithNegP <* spaceP                    -- Парсим число с возможным знаком минус (atomX) с пробелом после
  <*> floatWithNegP <* spaceP                    -- Парсим число с возможным знаком минус (atomY) с пробелом после
  <*> floatWithNegP <* spaceP                    -- Парсим число с возможным знаком минус (atomZ) с пробелом после
  <*> floatWithNegP <* spaceP                    -- Парсим число с возможным знаком минус (atomOccupancy) с пробелом после
  <*> floatWithNegP <* spaceP                    -- Парсим число с возможным знаком минус (atomTempFactor) с пробелом после
  <*> symbolP <* spaceP                          -- Парсим символ (atomElement) с пробелом после
  <*> optional (some (satisfyP (\c -> c == '+' || c == '-' || ('1' <= c && c <= '9')))) <* spaceP
                                                -- Парсим опциональный заряд атома (atomCharge) с пробелом после
    where
      floatWithNegP :: Parser Float
      floatWithNegP = negate <$> (satisfyP (=='-') *> floatP) <|> floatP

-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

connectP :: Parser PDBBond
connectP = PDBBond
  <$  spaceP
  <*> intP <* spaceP
  <*> optional intP <* spaceP
  <*> optional intP <* spaceP


modelP :: Parser PDBModel
modelP = PDBModel
  <$            stringP "MODEL" <*  skipToNewLine
  <*> many     (stringP "ATOM"   *> atomP)
  <*> (some    (stringP "CONECT" *> connectP) <|> pure [])
  <*  spaceP <* stringP "ENDMDL"
  where
    skipToNewLine :: Parser String
    skipToNewLine = many (satisfyP (/= '\n')) <* newLineP

pdbP :: Parser PDB
pdbP = PDB
   <$> many modelP <* spaceP
   <*  stringP "END" 

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

-- pure id <*> Nothing' = Nothing'       - Левая сторона
-- Nothing'             = Nothing'       - Правая сторона
--
-- pure id <*> (Just' x) = Just' id <*> Just' x = Just' (id x) - Левая сторона
-- Just' x               = Just' x       - Правая сторона
--
-- Закон 2 (Composition):
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- pure (.) <*> Nothing' <*> Just' f <*> Just' x = Nothing'                   - Левая сторона
-- Nothing' <*> (Just' f <*> Just' x)            = Nothing'                   - Правая сторона
--
-- pure (.) <*> Just' g <*> Just' f <*> Just' x  - Левая сторона
-- = Just' (g . f) <*> Just' x
-- = Just' (g . f $ x)
-- = Just' (g (f x))
-- = Just' g <*> Just' (f x)
-- = Just' g <*> (Just' f <*> Just' x)
--   
-- Just' g <*> (Just' f <*> Just' x) - Правая сторона
-- = Just' g <*> Just' (f x)
-- = Just' (g (f x))

-- Закон 3 (Homomorphism):
-- pure f <*> pure x = pure (f x)
--
-- pure g <*> pure x = Just' g <*> Just' x = Just' (g x) - Левая сторонаv
-- pure (g x)        = Just' (g x)  - Правая сторона

-- Закон 4 (Interchange):
-- u <*> pure y = pure ($ y) <*> u
--
-- Nothing' <*> pure y     = Nothing'   - Левая сторона
-- pure ($ y) <*> Nothing' = Nothing'   - Правая сторона
--
-- Just' f <*> pure y = Just' f <*> Just' y = Just' (f y) - Левая сторона
-- pure ($ y) <*> Just' f = Just' ($ y) <*> Just' f = Just' ($ y f) = Just' (f y) - Правая сторона


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

-- Закон 2 (Right Identity):
-- m >>= return = m
-- Just' a >>= return = return a = Just' a          - Левая сторона
-- Nothing' >>= return = return Nothing' = Nothing' - Правая сторона

-- Закон 3 (Associativity):
-- (m >>= f) >>= g = m >>= (\x -> f x >>= g)
-- (Nothing' >>= f) >>= g                 = Nothing' - Левая сторона
-- Nothing'         >>= (\x -> f x >>= g) = Nothing' - Правая сторона
--
-- (Just' x >>= f) >>= g = (f x) >>= g        - Левая сторона      
-- Just' x >>= (\x -> f x >>= g)              - Правая сторона
-- = (\x -> f x >>= g) x =
-- = f a >>= g                                - теперь все хорошо

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
--
-- pure id <*> Nil
-- -- Редукция
-- = Cons id Nil `append` (Nil <*> Nil)
-- -- По определению append
-- = Cons id Nil `append` Nil
-- -- По определению append
-- = Cons id Nil
-- -- По определению fmap
-- = fmap id Nil
-- -- По определению fmap
-- = Nil

-- Закон 2 (Composition):
-- pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
-- pure (.) <*> Cons f1 (Cons f2 Nil) <*> Cons g1 (Cons g2 Nil) <*> Cons x1 (Cons x2 Nil)
-- = fmap (.) (Cons f1 (Cons f2 Nil)) `append` (pure (.) <*> Cons g1 (Cons g2 Nil) <*> Cons x1 (Cons x2 Nil))  -- по определению <*>
-- = Cons (f1 . f2) Nil `append` (Cons (g1 . g2) Nil `append` (pure (.) <*> Cons x1 (Cons x2 Nil)))       -- по определению fmap и закону композиции для List
-- = Cons (f1 . f2) Nil `append` (Cons (g1 . g2) Nil `append` Cons (x1 . x2) Nil)                          -- по закону композиции для List
-- = fmap (f1 . f2) (Cons x1 (Cons x2 Nil))                                                               -- по определению fmap и закону композиции для List
-- = Cons (f1 . f2 $ x1) (fmap (f1 . f2) (Cons x2 Nil))                                                  -- по определению fmap и закону композиции для List
-- = Cons (f1 . f2 $ x1) (Cons (g1 . g2 $ x2) Nil)                                                      -- по определению fmap и закону композиции для List
-- = Cons (f1 (f2 x1)) (Cons (g1 (g2 x2)) Nil)                                                         -- по закону композиции для функций
-- = fmap f1 (Cons (f2 x1) (Cons (g2 x2) Nil))                                                          -- по определению fmap и закону композиции для List
-- = Cons (f1 (f2 x1)) (fmap g1 (Cons (g2 x2) Nil))                                                     -- по определению fmap и закону композиции для List
-- = Cons (f1 (f2 x1)) (Cons (g1 (g2 x2)) Nil)                                                         -- по закону композиции для функций
-- = fmap (f1 . f2) (Cons x1 (Cons x2 Nil))                                                             -- по определению fmap и закону композиции для List
-- = Cons (f1 . f2 $ x1) (fmap (f1 . f2) (Cons x2 Nil))                                                 -- по определению fmap и закону композиции для List
-- = Cons (f1 . f2 $ x1) (Cons (g1 . g2 $ x2) Nil)                                                      -- по определению fmap и закону композиции для List
-- = Cons (f1 (f2 x1)) (Cons (g1 (g2 x2)) Nil)                                                         -- по закону композиции для функций
-- = fmap f1 (Cons (f2 x1) (Cons (g2 x2) Nil))                                                          -- по определению fmap и закону композиции для List
-- = Cons (f1 (f2 x1)) (fmap g1 (Cons (g2 x2) Nil))                                                     -- по определению fmap и закону композиции для List
-- = Cons (f1 (f2 x1)) (Cons (g1 (g2 x2)) Nil)                                                         -- по закону композиции для функций
-- = fmap (f1 . f2) (Cons x1 (Cons x2 Nil))                                                             -- по определению fmap и закону композиции для List
-- = Cons (f1 . f2 $ x1) (fmap (f1 . f2) (Cons x2 Nil))                                                 -- по определению fmap и закону композиции для List
-- = Cons (f1 . f2 $ x1) (Cons (g1 . g2 $ x2) Nil)                                                      -- по определению fmap и закону композиции для List
-- = Cons (f1 (f2 x1)) (Cons (g1 (g2 x2)) Nil)                                                         -- по закону композиции для функций
-- после такого нужен курс реабилитации

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
-- (m >>= f) >>= g
-- = (Cons x xs >>= f) >>= g
-- = (f x `append` (xs >>= f)) >>= g
-- = (g (f x) `append` (xs >>= (\x -> f x >>= g)))
-- = (g (f x) `append` ((xs >>= f) >>= g))
-- = ((Cons x xs >>= f) >>= g)
-- = (m >>= (\x -> f x >>= g))

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
