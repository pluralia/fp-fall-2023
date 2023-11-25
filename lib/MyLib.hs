{-# LANGUAGE InstanceSigs #-}
module MyLib where


import           Control.Applicative
import qualified Data.Map.Strict as M
import           Parser
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Void (Void)
import           Data.Char (isSpace, isAlphaNum)
import           Data.Maybe   (isJust)
import           Control.Monad (void)

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

testMYParserIO :: FilePath -> Parsec Void String a -> IO Bool
testMYParserIO filePath parser = do
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

testIO :: FilePath -> Parser a -> IO (Maybe (a, String))
testIO filePath parser = do
    content <- readFile filePath         -- чтение из файла
    return $ Parser.runParser parser content    -- запуск парсера на содержимом

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

fastaP :: Parsec Void String Fasta
fastaP = do
  _ <- char '>'
  desc <- someTill anySingle newline
  seqq <- someTill (satisfy (\c -> c /= ';' && not (isSpace c))) (try (void (string ";\n")) <|> void newline <|> eof)
  return $ Fasta { description = desc, MyLib.seq = seqq }

fastaListP :: Parsec Void String [Fasta]
fastaListP = Text.Megaparsec.many fastaP


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
  deriving(Show)

newtype PDB' = PDB' [PDBModel'] 
  deriving(Show)

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
  cchainId <-  L.charLiteral <* space1
  rresidueNumber <- L.decimal <* space1
  xxCoord <- L.signed (void space) L.float  <* space1
  yyCoord <- L.signed (void space) L.float  <* space1
  zzCoord <- L.signed (void space) L.float  <* space1
  ooccupancy <- L.float <* space1
  ttemperatureFactor <- L.float <* space1
  eelement <- Text.Megaparsec.some (satisfy (\c -> isAlphaNum c || c `elem` ['-', '+'])) <* space
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
  _ <- string "MODEL" <* space1 *> (L.decimal :: Parsec Void String Int) <* newline
  atomss <- manyTill atomParser (string "ENDMDL" <* space)
  return $ PDBModel' atomss

-- Парсер для всего файла PDB
pdbParser' :: Parsec Void String PDB'
pdbParser' = do
  models <- manyTill modelParser' (string "END")
  return $ PDB' models


-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

connectParser :: Parsec Void String PDBBond
connectParser= do
  _ <- string "CONECT"
  bond1 <- Text.Megaparsec.Char.space *> L.decimal
  bond2 <- Text.Megaparsec.Char.space *> L.decimal <* space
  return $ PDBBond bond1 bond2

-- Парсер для секции MODEL, содержащей ATOM и CONNECT
modelParser :: Parsec Void String PDBModel
modelParser = do
  _ <- string "MODEL" <* space1 *> (L.decimal :: Parsec Void String Int)  <* newline
  atomss <- Text.Megaparsec.some (atomParser <* space)
  cconect <- manyTill connectParser (string "ENDMDL") <* space
  return $ PDBModel atomss cconect

-- Парсер для всего файла PDB где есть CONNECT
pdbParser :: Parsec Void String PDB
pdbParser = do
  models <- manyTill modelParser (string "END") 
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

-- Identity (pure id <*> v = v)

-- pure id <*> Nothing' = Nothing'
-- pure id <*> Just' f = Just' id <*> Just' f = Just' (id f) = Just' f

-- Composition (pure (.) <*> u <*> v <*> w = u <*> (v <*> w))

-- pure (.) <*> Just' u <*> Just' v <*> Just' w = Just' (.) <*> Just' u <*> Just' v <*> Just' w =
-- = Just' ((.) u) <*> Just' v <*> Just' w = Just' ((.u) v) <*> Just' w = Just' (u . v) <*> Just' w = Just' ((u . v) w) = Just'(u (v w))

-- Homomorphism (pure f <*> pure x = pure (f x))

-- pure f <*> pure x = Just' f <*> Just' x = Just' (f x)

-- Interchange (x <*> pure y = pure ($ y) <*> x)

-- Just' x <*> pure y = Just' x <*> Just' y = Just' (x y)

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Just' x >>= f = f x
  Nothing' >>= _ = Nothing'

-- -- Проверяем выполнение законов для Monad:

-- leftIdentity (return a >>= f = f a)

-- return a >>= f = Just' a >>= f  = f a

-- rightIdentity (m >>= return = m)

-- Just' m >>= return  = return m        = Just' m
-- Nothing' >>= return = return Nothing' = Nothing'

-- associativity (m >>= (\x -> f x >>= g) = (m >>= f) >>= g)

-- Just' m  >>= (\x -> f x >>= g) = (\x -> f x >>= g) m = f m >>= g
-- (Just' m  >>= f) >>= g                               = f m >>= g
-- Nothing' >>= (\x -> f x >>= g) = Nothing'
-- (Nothing' >>= f) >>= g         = Nothing'


---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?

data List a = Null | Cons a (List a)
  deriving (Show, Eq)

instance Semigroup (List a) where
  (<>) :: List a -> List a -> List a
  (<>)  Null       list = list
  (<>) (Cons x xs) list = Cons x (xs <> list)

instance Monoid (List a) where
  mempty :: List a
  mempty = Null

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Null         = Null
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Null

  (<*>) :: List (a -> b) -> List a -> List b
  (<*>)  Null         _           = Null
  (<*>)  _            Null        = Null
  (<*>) (Cons fx fs) (Cons x xs) = Cons (fx x) (fmap fx xs <> (fs <*> xs))

-- Доказательства законов для Applicative:

-- Identity (pure id <*> v = v)

-- pure id <*> Null = Cons id Null <*> Null = Cons (id) (fmap id Null) = | fmap для Null возвращает Null | = Cons id Null = Null
-- Null <*> pure id = Null <*> Cons id Null = Null -- так как первый аргумент Null

-- Composition (pure (.) <*> u <*> v <*> w = u <*> (v <*> w))

-- Это Left Composition
-- pure (.) <*> u <*> v <*> w = Cons (.) Null <*> u <*> v <*> w = | поскольку Cons (.) Null эквивалентен pure (.) для нашего типа List | =
-- = fmap (.) u <*> v <*> w = fmap (.) (Cons u us) <*> v <*> w = Cons (u .) (fmap (.) us) <*> v <*> w = Cons (u .) (fmap (.) us <*> v) <*> w
-- = (Cons (u .) (fmap (.) us <*> v) <*> w) = (u <*> (v <*> w))

-- Это Right Composition
-- pure (.) <*> u <*> v <*> w = Cons (.) Null <*> u <*> v <*> w = fmap (.) u <*> v <*> w = fmap (.) (Cons u us) <*> v <*> w =
-- = Cons (u .) (fmap (.) us) <*> v <*> w = Cons (u .) (fmap (.) us <*> v) <*> w = Cons (u .) ((fmap (.) us <*> v) <*> w) =
-- = Cons (u .) (u <*> (v <*> w)) = (u <*> (v <*> w))

-- Homomorphism (pure f <*> pure x = pure (f x))

-- pure f <*> pure x = Cons f Null <*> Cons x Null = Cons (f x) (fmap f Null) = Cons (f x) Null = pure (f x)

-- Interchange (u <*> pure y = pure ($ y) <*> u)

-- Это Left
-- u <*> pure y = Cons f fs <*> pure y = Cons f fs <*> Cons y Null = Cons (f y) (fmap f Null) =
-- = Cons (f y) Null  = pure ($ y) <*> Cons f fs  = pure ($ y) <*> u

-- Это Right
-- pure ($ y) <*> u = Cons ($) Null <*> u = Cons ($) Null <*> Cons f fs = Cons ($ f) (fmap ($) fs) <*> u =
-- = Cons ($ f) (fmap ($) fs <*> u) = Cons ($ f) (fmap ($) fs <*> u) = Cons ($ f) (u <*> fs) = u <*> Cons ($ f) fs = u <*> pure y

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (>>=)  Null        _ = Null
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

-- Доказательство законов для Monad

-- Left Identity (return a >>= f = f a)
-- return a >>= f = Cons a Null >>= f = f a <> (Null >>= f)  = f a <> Null = f a

-- Right Identity (m >>= return = m)
-- m >>= return = Cons x xs >>= return = return x <> (xs >>= return) = Cons x Null <> (xs >>= return) = Cons x Null = Cons x xs

-- Associativity ((m >>= f) >>= g = m >>= (\x -> f x >>= g))
-- (m >>= f) >>= g = (Cons x xs >>= f) >>= g = (f x <> (xs >>= f)) >>= g = (f x >>= g) <> (xs >>= f >>= g) = (f x >>= g) <> (xs >>= (\x -> f x >>= g))


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

-- Добавил реализацию монады, ее не было изначально 
instance Monad (Either' a) where
    (>>=) :: Either' a b -> (b -> Either' a c) -> Either' a c
    (>>=) (Left'  e) _ = Left' e
    (>>=) (Right' r) f = f r

-- Доказательства законов:
-- Для Applicative:

-- Identity (pure id <*> v = v)

-- Для Right' y
-- pure id <*> x == Right' id <*> Right' y  == Right' (id y) == Right' y == x
-- Для Left' 
-- pure id <*> x == Right' id <*> Left'  y  == Left' y                   == x


-- Composition (pure (.) <*> u <*> v <*> w = u <*> (v <*> w))

-- Пусть u = Left' x, v = Left' y, w = Left' z, тогда:
-- pure (.) <*> Left' x <*> Left' y <*> Left' z = Right' (.) <*> Left' x <*> Left' y <*> Left' z = Left' x <*> Left' y <*> Left' z = Left' x
-- При этом также:
-- Left' x <*> (Left' y <*> Left' z) = Left' x <*> Left' y = Left' x

-- Пусть u = Right' x, v = Right' y, w = Right' z, тогда:
-- pure (.) <*> Right' x <*> Right' y <*> Right' z = Right' (.) <*> Right' x <*> Right' y <*> Right' z  =
-- = Right' ((.) x)  <*> Right' y <*> Right' z = Right' ((. x) y)  <*> Right' z = 
-- = Right' (x . y) <*> Right' z =  Right' ((x . y) z) = Right'(x (y z))
-- При этом также:
-- Right' x <*> (Right' y <*> Right' z) = Right' x <*> Right' (y z) = Right' (x (y z))


-- 3. Homomorphism (pure f <*> pure x = pure (f x))

-- pure f <*> pure x == Right' f <*> Right' x == Right' (f x) == pure (f x)


-- 4. Interchange (x <*> pure y = pure ($ y) <*> x)

-- Для Left'
-- Left' x <*> pure y     == Left' x      <*> Right' y  == Left' x
-- pure ($ y) <*> Left' x == Right' ($ y) <*> Left'  x  == Left' x

-- Для Right'
-- Right' x <*> pure y == Right' x     <*> Right' y                     == Right' (x y)
-- pure ($ y) <*> x    == Right' ($ y) <*> Right' x == Right' (($ y) x) == Right' (x y)

-- Для Monad

-- Left identity (return a >>= k = k a)
-- Для Left' 
-- return a >>= Left'  = Right' a >>= Left'  = Left' a
-- Для Right'
-- return a >>= Right' = Right' a >>= Right' = Right' a

-- Right identity (m >>= return = m)
-- Для Left' x
-- Left' x  >>= return = Left' x  >>= Right' = Left' x
-- Для Right' x
-- Right' x >>= return = Right' x >>= Right' = Right' x

-- Associativity ((m >>= f) >>= g = m >>= (\x -> f x >>= g))
-- Для m = Left' x, f = Left' y, g = Left' z
-- Left' x >>= (\x -> Left' y x >>= Left' z) = Left' x 

-- Для m = Right' x, f = Right' y, g = Right' z
-- Right' x >>= (\x -> Right' y x >>= Right' z) = (\x -> Right' y x >>= Right' z) y = Right' x y >>= Right' z = x y z


-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)

{-
Стрелка Клейсли для монады m и типа a обозначается как a -> m b. 
Это функция, которая принимает значение типа a и возвращает вычисление в монаде m, результат которого имеет тип b.
-}

-------------------------------------------------------------------------------
