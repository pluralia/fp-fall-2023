{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module MyLib where

import           Data.Ix
import           Control.Applicative
import           Data.List
import           Data.Maybe (isJust)
import           Parser
import           Data.Char (digitToInt, isAlphaNum, isAlpha, isSpace, isDigit)
-------------------------------------------------------------
-- HW 3 -- про паттерны и инстансы классов, для отработки взяла еще и первое задание с несделанным Ix

-- Task 1. ChurchNumber + Ix

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show, Eq, Ord)

-- это взяла из реализованного в 3 ДЗ, потому что нужно для определения Ix
instance Num ChurchNumber where
    fromInteger n
        | n < 0     = error "negative!"
        | n == 0    = Zero
        | otherwise = Succ (fromInteger (n - 1))

    (+) m Zero     = m
    (+) m (Succ n) = Succ (m + n)

    (-) m Zero            = m
    (-) Zero     _        = Zero
    (-) (Succ m) (Succ n) = m - n

    (*) Zero _        = Zero
    (*) _    Zero     = Zero
    (*) m    (Succ n) = m + (m * n)

    abs             = id
    signum Zero     = Zero
    signum (Succ _) = Succ Zero

-- Minimal complete definition - range, (index | unsafeIndex), inRange
-- это так должно выглядеть?
instance Ix ChurchNumber where
    range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
    range (l,r) | l > r = []
                | l == r = [r]
                | otherwise = l : range (Succ l, r)

    index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
    index (l, r) i | l > r = error "Wrong limits"
                   | i < l || i > r = error "Number out of range"
                   | i == l = 0
                   | otherwise = 1 + index (l, r) (i - 1)
    
    inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool
    inRange (l, r) i = i >= l && i <= r



-- Task 2. in-order обход дерева -- создание своих типов данных 
{-
Tree - идентификатор типа
Node - конструктор типа с записями типов данных с именнованными полями
-}
data Tree a = Node
  { value :: a
  , children :: [Tree a]
  }

{-
InOrder - идентификатор типа - 
In - конструктор типа с записями типов данных с неименнованными полями
-}
newtype InOrder a = In (Tree a)

{-
я попробовала переделать обход и вроде выглядит теперь верно, но при попытке проверить в ghci, 
у меня не получается задать через let
но тесты проходит
-}
instance Show a => Show (InOrder a) where
    show (In tree) = inOrderSubtree [tree]
      where
        inOrderSubtree :: Show a => [Tree a] -> String
        inOrderSubtree [] = ""
        inOrderSubtree (Node val subtree : ts) = show val ++ inOrderSubtree subtree ++ inOrderSubtree ts

----------------------------------------------------------------------------

-- HW6

-- Task 1. Parsers


-- | 1.d Парсит заданную строку 
{-
моя реализация после Ваших комментариев

stringP :: String -> Parser String
stringP str = Parser go
  where
    go :: String -> Maybe (String, String)
    go pattern | str == "" = Nothing
               | otherwise = (:) <$> satisfyP (== pattern) <*> stringP

моя ошибка в том, что satisfyP принимает только 1 символ, а я его заставляю взять весь паттерн
в книжке "Изучай haskell..." указывается функция `isPrefixOf`, которая делает то, что я хочу.
Так как ее не запрещалось использовать, то
-}
{-
hlint выдает ошибку 
Error: Parse error: on input `pattern'
Found:
    stringP :: String -> Parser String
  > stringP pattern = Parser go
      where
        go :: String -> Maybe (String, String)
я не понимаю, откуда она
-}

stringP :: String -> Parser String
stringP pattern = Parser go
  where
    go :: String -> Maybe (String, String)
    go str 
      | pattern `isPrefixOf` str = Just (pattern, drop (length pattern) str)
      | otherwise = Nothing

----------------------------------------------------------------------------

-- HW7

-- | Для чтения содержимого фалов в заданиях 2 и 3 используйте эту функцию
-- у меня не получается запустить эту функцию
testIO :: FilePath -> Parser a -> IO (Maybe (a, String))
testIO filePath parser = do
    content <- readFile filePath         -- чтение из файла
    return $ runParser parser content    -- запуск парсера на содержимом

-- | Чтобы использовать файлы для тестов, воспользуйтесь этой функцией
--   Здесь мы просто проверяем, что результат парсинга не Nothing
-- 
testParserIO :: FilePath -> Parser a -> IO Bool
testParserIO filePath parser = isJust <$> testIO filePath parser

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
{- комментарии:
1. разделители лучше парсить в основном парсере, а не в парсерах частей 
добавлены commP в основной парсер fastaP
newLineP перенесены из fastaListP

2. Как думаешь, чем лучше парсинг разделителей в основном парсере?
видимо читаемость - я так понимаю, что в haskell  это чуть ли не основной критерий удобства функции
-}
fastaP :: Parser Fasta
fastaP = Fasta 
       <$> (many commP *> descrP)
       <*> (many commP *> seqP)
       <* many commP

  where
    commP :: Parser String
    commP = satisfyP (== ';') 
            *> many (satisfyP (/='\n'))
            <* many newLineP

    descrP :: Parser String
    descrP = satisfyP (== '>')
             *> many (satisfyP (/= '\n'))

    seqP :: Parser [Acid]
    seqP = concat 
           <$> many (satisfyP (\c -> c /= ';' && c /= '>')
           *> many (satisfyP (/= '\n'))
           <* many newLineP)

fastaListP :: Parser [Fasta]
fastaListP = many fastaP

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
-- https://www.wwpdb.org/documentation/file-format-content/format33/sect9.html#MODEL


-- я не разобралась, как это парсить

data PDBAtom = PDBAtom 
    { 
        record_name :: String
      , serial_num :: Int
      , name       :: String
      , altLoc     :: Char
      , resName    :: String
      , chainID    :: Char
      , resSeq     :: Int
      , iCode      :: Maybe Char -- опционально
      , xx         :: Float
      , yy         :: Float
      , zz         :: Float
      , occupancy  :: Float
      , tempFactor :: Float
      , element    :: Char
      , charge     :: Maybe String --- опционально
    } deriving (Eq, Show)

-- | Тип, представляющий из себя CONNECT
-- https://www.wwpdb.org/documentation/file-format-content/format33/sect10.html#CONECT
-- я так поняла, что одна связь должна быть, а  оставшиеся 3 не обязательны

data PDBBond = PDBBond 
    {
        con_name  :: String
      , atom_num  :: Int 
      , bondedA1  :: Int
      , bondedA2  :: Maybe Int
      , bondedA3  :: Maybe Int
      , bondedA4  :: Maybe Int
    }
    deriving (Eq, Show)

data PDBModel
  = PDBModel
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      } deriving (Eq, Show)

-- | PDB-файл
--
newtype PDB = PDB [PDBModel] deriving (Eq, Show)

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.


atomP :: Parser PDBAtom
atomP = PDBAtom
  <$> stringP "ATOM" <* spaceP -- хочу парсить только строчку, без слова ATOM (по аналогии с fasta - чтобы было понятно, какую секцию обрабатываю)
  <*> intP <* spaceP -- номер атома
  <*> symbolsP <* spaceP -- имя атома
  <*> satisfyP isAlpha <* spaceP -- altLoc - одна буква
  <*> many (satisfyP isAlpha) <* spaceP  -- resName  - много букв - many
  <*> satisfyP isAlpha <* spaceP --chainID - одна буква
  <*> intP <* spaceP --resSeq - одна цифирка
  <*> optional (satisfyP isAlpha) <* spaceP -- iCode - опционально 1 буква
  <*> myFloatP <* spaceP --x
  <*> myFloatP <* spaceP --y
  <*> myFloatP <* spaceP --z
  <*> myFloatP <* spaceP --occupancy
  <*> myFloatP <* spaceP --tempFactor
  <*> symbolP <* spaceP --element
  <*> optional (some (satisfyP (`elem` ['+', '-'] <> ['1'..'9']))) <* spaceP -- charge опционально несколько символов
    where
      -- честно спросила у ребят, что не так с Float - наш floatP умеет только в + числа, поэтому надо проверять '-' отдельно
      myFloatP :: Parser Float
      myFloatP = negate <$> (satisfyP (=='-') *> floatP) <|> floatP


-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

connectP :: Parser PDBBond
connectP = PDBBond
  <$> stringP "CONECT" <* spaceP -- аналогично, хочу парсить только строчку, без слова "CONECT"
  <*> intP <* spaceP
  <*> intP <* spaceP
  -- я хотела здесь через some, но оно не заработало
  <*> optional intP <* spaceP
  <*> optional intP <* spaceP
  <*> optional intP <* spaceP

modelP :: Parser PDBModel
modelP = PDBModel
  <$  stringP "MODEL" <* many (satisfyP (/='\n')) <* newLineP
  <*> many (atomP)
  <*> (some connectP <|> pure [])
  <*  spaceP <* stringP "ENDMDL"

pdbP :: Parser PDB
pdbP = PDB
   <$> many modelP <* spaceP
   <*  stringP "END" 


-------------------------------------------------------------------------------

-- 4. Monad Parser (0,5 балла)
--    (можно без док-ва законов)

-- | Определен в файле Parser.hs
--   newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
-- здесь cabal ругается на объявление в этом файле, но это задание, так что оставила
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) p f = Parser $ \str -> case runParser p str of
        Nothing -> Nothing
        Just (strs, stre) -> runParser (f strs) stre

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

-- паттерн матчинг
instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  (<*>) Nothing' _ = Nothing'
  (<*>) _ Nothing' = Nothing'
  (<*>) (Just' f) (Just' x) = Just' (f x)

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  (>>=) Nothing' _ = Nothing'
  (>>=) (Just' x) f = f x

---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

-- по аналогии с Maybe'

data List a = Empty | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  (<>) Empty somelist = somelist
  (<>) (Cons x xs) somelist = Cons x ((<>) xs somelist)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Empty

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>) _ Empty = Empty
  (<*>) Empty _ = Empty
  (<*>) (Cons f fs) (Cons x xs) = Cons (f x) (fs <*> xs)
-- последний (<*>) соединяет списки функций и значений, получается zip

{-
здесь у меня не захотел объединять с помощью <>, ++ или : без инстанса Semigroup
поэтому определила выше -}
instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=) Empty _ = Empty
  (>>=) (Cons x xs) f = f x <> ((>>=) xs f)
---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

data Either' a b = Left' a | Right' b deriving (Eq, Show)

instance Functor (Either' a) where
  fmap :: (b -> c) -> Either' a b -> Either' a c
  fmap _ (Left' x) = Left' x
  fmap f (Right' x) = Right' (f x)

instance Applicative (Either' a) where
  pure :: b -> Either' a b
  pure x = Right' x

  (<*>) :: Either' a (b -> c) -> Either' a b -> Either' a c
  (<*>) (Left' x) _ = Left' x
  (<*>) _  (Left' x) = Left' x
  (<*>) (Right' f) (Right' x) = Right' (f x)

instance Monad (Either' a) where
  (>>=) :: Either' a b -> (b -> Either' a c) -> Either' a c
  (>>=) (Left' x) _ = Left' x
  (>>=) (Right' x) f = f x
-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)
{- начало обсуждения -> https://github.com/pluralia/fp-fall-2023/pull/51#discussion_r1398121565

1. монада - это не функция, не тип данных, не класс. Что это тогда? Как его понимать?

2. "сама стрелка Клейсли -- это функция, которая берет на вход значение типа a и модифицирует 
его в значение типа b, а затем заворачивает в монаду m"
функция может быть любой? а как мы ее подаем в эту стрелку? 
-}