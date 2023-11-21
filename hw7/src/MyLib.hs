{-# LANGUAGE InstanceSigs #-}

module MyLib where

import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import           Parser
import           Test.QuickCheck

-------------------------------------------------------------------------------

-- 1. Парсер строки CSV формата with Maybe (0,5 балла)

data Value
  = IntValue Int
  | FloatValue Float
  | StringValue String
  deriving (Eq, Show)

valueP :: Parser Value
valueP =  IntValue    <$> intP
      <|> FloatValue  <$> floatP
      <|> StringValue <$> symbolsP

newtype Row = Row (M.Map String (Maybe Value))
  deriving (Show)

-- | напишите парсер строки: заметьте, что теперь строка -- Map в `Maybe Value`
--
rowP :: [String] -> Parser Row
rowP cNames = Row . M.fromList . zip cNames <$> sepBy (satisfyP (== ',')) (optional valueP)

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

fastaDescriptionP :: Parser String
fastaDescriptionP = satisfyP (== '>') *> many (satisfyP (/= '\n'))

fastaSeqP :: Parser [Acid]
fastaSeqP = satisfyP (\c -> c /= '>' && c /= ';') *> many (satisfyP (/= '\n')) <* many newLineP

fastaSeqsP :: Parser [Acid]
fastaSeqsP = concat <$> many fastaSeqP 

fastaCommentP :: Parser String
fastaCommentP = satisfyP (== ';') *> many (satisfyP (/= '\n')) <* many newLineP

fastaP :: Parser Fasta
fastaP = Fasta 
  <$> (many fastaCommentP *> fastaDescriptionP) 
  <*> (many fastaCommentP *> fastaSeqsP)
  <* many fastaCommentP

fastaListP :: Parser [Fasta]
fastaListP = many fastaP

testIO0 :: IO (Maybe ([Fasta], String))
testIO0 = do
    content <- readFile "src/test.fasta"
    return $ runParser fastaListP content

-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = PDBAtom
    {
      serial     :: Int          -- номер атома [7-11]
    , name       :: String       -- название атома [13-16]
    , altLoc     :: Maybe String -- альтернативное расположение [17]
    , resName    :: String       -- название остатка [18-20]
    , chainID    :: Char         -- идентификатор цепочки [22]
    , resSeq     :: Int          -- номер остатка [23-26]
    , iCode      :: Maybe String -- код вставки [27]
    , x          :: Float        -- координата x [31-38]
    , y          :: Float        -- координата y [39-46]
    , z          :: Float        -- координата z [47-54]
    , occupancy  :: Float        -- занимаемый объем [55-60]
    , tempFactor :: Float        -- фактор температуры [61-66]
    , element    :: String       -- элемент [77-78]
    , charge     :: Maybe String -- заряд [79-80]
    }deriving (Eq, Show)


-- | Тип, представляющий из себя CONNECT
--
data PDBBond = PDBBond
    {
      atom1       :: Int   -- номер первого атома
    , bondedAtoms :: [Int] -- номера атомов, связанных с первым
    }deriving (Eq, Show)

-- | Тип, представляющий из себя MODEL
--
data PDBModel
  = PDBModel
      { 
        model  :: Int       -- номер модели
      , atoms  :: [PDBAtom] -- атомы из секции ATOM
      , bonds  :: [PDBBond] -- связи из секции CONNECT
      }deriving (Eq, Show)

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.

count :: Int -> Parser String
count n = Parser $ \s -> if length s < n then Nothing else Just (splitAt n s)

-- Я не знаю есть ли этому готовая альтернатива.
-- мне нужно было, чтобы делать так:
-- ghci> runParser (floatP <.> count 6) "0.12536567.0 aabd"
-- Just (0.1253,"6567.0 aabd")

infixl 4 <.>
(<.>) :: Parser b -> Parser String -> Parser b
bP <.> aP = Parser $ \s -> case runParser aP s of
  Nothing -> Nothing
  Just (a, rest) -> case runParser bP a of
    Nothing -> Nothing
    Just (b, _) -> Just (b, rest)


atomP :: Parser PDBAtom
atomP = do
  _ <- stringP "ATOM  "
  serial' <- (spaceP *> intP) <.> count 5
  _ <- count 1
  name' <- (spaceP *> symbolsP <* spaceP) <.> count 4
  altLoc' <- spaces' $ count 1
  resName' <- count 3
  _ <- count 1
  chainID' <- symbolP
  resSeq' <- (spaceP *> intP) <.> count 4
  iCode' <- spaces' $ count 1
  _ <- count 3
  x' <- (spaceP *> floatP) <.> count 8
  y' <- (spaceP *> floatP) <.> count 8
  z' <- (spaceP *> floatP) <.> count 8
  occupancy' <- (spaceP *> floatP) <.> count 6
  tempFactor' <- (spaceP *> floatP) <.> count 6
  _ <- count 10
  element' <- (spaceP *> symbolsP) <.> count 2
  charge' <- spaces' $ count 2
  _ <- many newLineP
  return $ PDBAtom serial' name' altLoc' resName' chainID' resSeq' iCode' x' y' z' occupancy' tempFactor' element' charge'
  

-- если res парсера состоит из пробелов кидать Nothing
-- иначе Just res
spaces' :: Parser String -> Parser (Maybe String)
spaces' p = Parser f
  where
    f s = case runParser p s of
      Nothing -> Nothing
      Just (a, rest) -> case runParser spaceP a of
        Just (_, "") -> Just (Nothing, rest)
        _ -> Just (Just a, rest)

        
-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

bondP :: Parser PDBBond
bondP = do
  _ <- stringP "CONECT"
  atom1' <- (spaceP *> intP) <.> count 5
  bondedAtoms' <- many ((spaceP *> intP) <.> count 5)
  _ <- many newLineP
  return $ PDBBond atom1' bondedAtoms'

modelP :: Parser PDBModel
modelP = PDBModel -- one parser to rule them all
  <$> (stringP "MODEL" *> spaceP *> intP <* newLineP) 
  <*> many atomP 
  <*> many bondP 
  <* stringP "ENDMDL" <* newLineP

modelsP :: Parser [PDBModel]
modelsP = some modelP <* stringP "END" <* many newLineP

testIOnobonds :: IO (Maybe ([PDBModel], String))
testIOnobonds = do
    content <- readFile "src/only_atoms.pdb"
    return $ runParser modelsP content

testIObonds :: IO (Maybe ([PDBModel], String))
testIObonds = do
    content <- readFile "src/atoms_with_bonds.pdb"
    return $ runParser modelsP content   

-------------------------------------------------------------------------------

-- 4. Monad Parser (0,5 балла)
--    (можно без док-ва законов)

-- | Определен в файле Parser.hs
--   newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- instance Monad Parser where
--   (>>=) :: Parser a -> (a -> Parser b) -> Parser b
--   (>>=) aP f = Parser $ \s -> case runParser aP s of
--     Nothing -> Nothing
--     Just (result, rest) -> runParser (f result) rest
-- 
-- Переехал в Parser.hs. Компилятор ругается, если оставить это здесь

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
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' $ f a


instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  (<*>) Nothing' _ = Nothing'
  (<*>) _ Nothing' = Nothing'
  (<*>) (Just' f) (Just' a) = Just' $ f a

-- доказательство законов методом перебора. А можно юзать Test.QuickCheck?)))
--
-- 1. pure id <*> v = v
-- pure id <*> Nothing' = Nothing'
-- pure id <*> Just' a = Just' $ id a = Just' a
--
-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure (.) <*> Nothing' <*> v <*> w = Nothing' = Nothing' <*> (v <*> w)
-- pure (.) <*> u <*> Nothing' <*> w = Nothing' = u <*> Nothing' <*> w
-- pure (.) <*> u <*> v <*> Nothing' = Nothing' = u <*> (v <*> Nothing')
-- pure (.) <*> Just' f <*> Just' g <*> Just' a = 
--  = Just' $ (.) f <*> Just' g <*> Just' a =
--   = Just' $ f . g <*> Just' a =
--    = Just' $ f (g a) =
--     = Just' $ f <$> Just' (g a) =
--      = Just' $ f <$> (Just' g <*> Just' a) =
--       = Just' f <*> (Just' g <*> Just' a) 
--
-- 3. pure f <*> pure x = pure (f x)
-- Just' f <*> Just' x = Just' $ f x = pure (f x)
-- pure f <*> Nothing' = Nothing' = pure (f Nothing')
-- pure Nothing' <*> Just' x = Nothing' = pure (Nothing' x)
--
-- 4. u <*> pure y = pure ($ y) <*> u 
-- как я понял ($ y) - это функция, которая принимает функцию и возвращает ее применение к y
-- Nothing' <*> pure y = Nothing' = pure ($ y) <*> Nothing'
-- Just' f <*> pure y = Just' $ f y = pure ($ y) <*> Just' f


instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  (>>=) Nothing' _ = Nothing'
  (>>=) (Just' a) f = f a

  return :: a -> Maybe' a
  return = pure

-- 1. return a >>= f = f a
-- return a >>= f = Just' a >>= f = f a
--
-- 2. m >>= return = m
-- Nothing' >>= return = Nothing'
-- Just' a >>= return = return a = Just' a
--
-- 3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- k :: a -> Maybe' b
-- h :: b -> Maybe' c
-- \x -> (k x >>= h) :: a -> Maybe' c
-- Nothing' >>= (\x -> k x >>= h) = Nothing' = (Nothing' >>= k) >>= h
-- Just' a >>= (\x -> k x >>= h) = k a >>= h = (Just' a >>= k) >>= h

---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

data List' a = Nil' | Cons' a (List' a)
  deriving (Show, Eq)


instance Semigroup (List' a) where
  (<>) :: List' a -> List' a -> List' a
  (<>) Nil' as = as
  (<>) (Cons' a as) bs = Cons' a (as <> bs)


instance Functor List' where
  fmap :: (a -> b) -> List' a -> List' b
  fmap _ Nil' = Nil'
  fmap f (Cons' a as) = Cons' (f a) (fmap f as)


instance Applicative List' where
  pure :: a -> List' a
  pure a = Cons' a Nil'

  (<*>) :: List' (a -> b) -> List' a -> List' b
  (<*>) Nil' _ = Nil'
  (<*>) _ Nil' = Nil'
  (<*>) (Cons' f fs) as = fmap f as <> (fs <*> as) -- матчим каждый с каждым

-- 1. pure id <*> v = v
-- pure id <*> Nil' = Nil'
-- pure id <*> Cons' a as = 
--  = Cons' (id a) (pure id <*> as) = Cons' a as 
--
-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure (.) <*> Cons' f fs <*> Cons' g gs <*> Cons' a as =
--  = Cons' (.) Nil' <*> Cons' f fs <*> Cons' g gs <*> Cons' a as =
--   = Cons' ((.) f) ((.) <$> fs) <*> Cons' g gs <*> Cons' a as =
-- Это какой-то ужас
--
-- 3. pure f <*> pure x = pure (f x)
-- Cons' f Nil' <*> Cons' x Nil' = Cons' (f x) Nil' = pure (f x)
--
-- 4. u <*> pure y = pure ($ y) <*> u
-- Nil' <*> Cons' y Nil' = Nil' = pure ($ y) <*> Nil'
-- Cons' f fs <*> Cons' y Nil' = 
--  = Cons' (f y) (fs <*> Cons' y Nil') =
--   = Cons' (f y) (fs <*> pure y) =
--    = Cons' (f y) (pure ($ y) <*> fs) =
--     = pure ($ y) <*> Cons' f fs



instance Monad List' where
  (>>=) :: List' a -> (a -> List' b) -> List' b
  (>>=) Nil' _ = Nil'
  (>>=) (Cons' a as) f = f a <> (as >>= f)

  return :: a -> List' a
  return = pure

-- 1. return a >>= f = f a
-- return a >>= f = Cons' a Nil' >>= f = f a
--
-- 2. m >>= return = m
-- Nil' >>= return = Nil'
-- Cons' a as >>= return = 
--  = return a <> (as >>= return) =
--   = Cons' a Nil' <> (as >>= return) =
--    = Cons' a Nil' <> as =
--     = Cons' a as
--
-- 3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- Nil' >>= (\x -> k x >>= h) = Nil' = (Nil' >>= k) >>= h
--
-- Cons' a as >>= (\x -> k x >>= h) =
--  = (k a >>= h) <> (as >>= (\x -> k x >>= h)) = тут, наверное, надо сказать про индукцию
--   = (k a >>= h) <> ((as >>= k) >>= h) ==
--    = (k a <> (as >>= k)) >>= h =
--     = (Cons' a as >>= k) >>= h

---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

data Either' a b = Left' a | Right' b
  deriving (Show, Eq)


instance Functor (Either' a) where
  fmap :: (b -> c) -> Either' a b -> Either' a c
  fmap _ (Left' a) = Left' a
  fmap f (Right' b) = Right' $ f b


instance Applicative (Either' a) where
  pure :: b -> Either' a b
  pure = Right'

  (<*>) :: Either' a (b -> c) -> Either' a b -> Either' a c
  (<*>) (Left' a) _ = Left' a
  (<*>) _ (Left' a) = Left' a
  (<*>) (Right' f) (Right' b) = Right' $ f b

-- 1. pure id <*> v = v
-- pure id <*> Left' a = Left' a
-- pure id <*> Right' b = Right' $ id b = Right' b

-- 2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure (.) <*> Left' a <*> _ <*> _ = Left' a = Left' a <*> (_ <*> _)
-- pure (.) <*> Right' a <*> Left' b <*> _ = Left' b = Right' a <*> (Left' b <*> _)
-- pure (.) <*> Right' a <*> Right' b <*> Left' c = Left' c = Right' a <*> (Right' b <*> Left' c)
-- pure (.) <*> Right' a <*> Right' b <*> Right' c =
--  = Right' (.) <*> Right' a <*> Right' b <*> Right' c =
--   = Right' $ (.) a <*> Right' b <*> Right' c =
--    = Right' $ a . b <*> Right' c = 
--     = Right' $ (a . b) c =
--      = Right' $ a (b c) =
--       = Right' a <*> Right' (b c)
--        = Right' a <*> (Right' b <*> Right' c)
--
-- 3. pure f <*> pure x = pure (f x)
-- Right' f <*> Right' x = Right' $ f x = pure (f x)
--
-- 4. u <*> pure y = pure ($ y) <*> u
-- Left' a <*> Right' y = Left' a = pure ($ y) <*> Left' a
-- Right' b <*> Right' y = Right' $ f y = pure ($ y) <*> Right' b


instance Monad (Either' a) where
  (>>=) :: Either' a b -> (b -> Either' a c) -> Either' a c
  (>>=) (Left' a) _ = Left' a
  (>>=) (Right' b) f = f b

  return :: b -> Either' a b
  return = pure

-- 1. return a >>= f = f a
-- return a >>= f = Right' a >>= f = f a
--
-- 2. m >>= return = m
-- Left' a >>= return = Left' a
-- Right' b >>= return = return b = Right' b
--
-- 3. m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- Left' a >>= (\x -> k x >>= h) = Left' a = (Left' a >>= k) >>= h
--
-- Right' b >>= (\x -> k x >>= h) = 
--  = (\x -> k x >>= h) b = 
--   = k b >>= h = (Right' b >>= k) >>= h

-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)

-- стрелка Клейсли -- используется для обозначения функции, 
-- которая принимает значение и возвращает монаду:
-- a -> m b, где a и b - это типы, а m - это монада.

-------------------------------------------------------------------------------
