{-# LANGUAGE InstanceSigs #-}

module MyLib where
import           Control.Applicative
import qualified Data.Map.Strict as M
import           Data.Maybe (isJust)
import           Parser
import           Data.List
import           Data.Functor
import           Data.Char

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
rowP :: [String] -> Parser Row
rowP cNames = Row . M.fromList . zip cNames <$> sepBy (satisfyP (== ',')) (optional valueP)

-------------------------------------------------------------------------------

-- | Для чтения содержимого фалов в заданиях 2 и 3 используйте эту функцию
--
testIO' :: FilePath -> Parser a -> IO (Either String (a, String))
testIO' filePath parser = do
    content <- readFile filePath         -- чтение из файла
    let result = runParser parser content
    return $ case result of
        Nothing -> Left ("Parsing failed, input was: " ++ content)
        Just partialResult -> Right partialResult


testIO :: FilePath -> Parser a -> IO (Maybe (a, String))
testIO filePath parser = do
    content <- readFile filePath         -- чтение из файла
    return $ runParser parser content    -- запуск парсера на содержимом

-- | Чтобы использовать файлы для тестов, воспользуйтесь этой функцией
--   Здесь мы просто проверяем, что результат парсинга не Nothing
-- 
testParserIO :: FilePath -> Parser a -> IO Bool
testParserIO filePath parser = isJust <$> testIO filePath parser
-- testParserIO :: FilePath -> Parser a -> IO (Maybe (a, String))
-- testParserIO filePath parser = testIO filePath parser

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
fastaListP = many fastaP

fastaP :: Parser Fasta
fastaP = Fasta <$> (many commentP *> descriptionP <* many commentP) <*> (aminoAcidP <* many commentP)

descriptionP :: Parser String
descriptionP = satisfyP (== '>')  *> some (satisfyP isLetter) <* many (satisfyP (/= '\n'))

aminoAcidP :: Parser String
aminoAcidP = some (satisfyP isLetter) <* many (satisfyP (\c -> c /= '>' && c /= ';'))

commentP :: Parser ()
commentP = (satisfyP (== ';') *> many (satisfyP (/= '\n'))) Data.Functor.$> ()
-------------------------------------------------------------------------------

-- 3. Парсер PDB (3,5 балла)

-- PDB -- формат для хранения информации о трёхмерных структурах молекул.
-- Cпецификация: https://www.wwpdb.org/documentation/file-format-content/format33/v3.3.html
-- Она довольно большая, но мы не будем парсить всё, что в ней есть.
-- Во всем задании нам понадобится парсить только секции MODEL, ATOM и CONNECT

-- | Тип, представляющий из себя ATOM
--
data PDBAtom = PDBAtom
  { atomSerial :: Int    -- номер атома
  , atomName :: String -- имя атома
  , atomResName :: String -- имя остатка
  , atomChainID :: Char  -- идентификатор цепи
  , atomResSeq :: Int    -- последовательность остатков
  , atomPos :: (Float, Float, Float) -- позиция атома в трехмерном пространстве
  , atomOccupancy :: Float -- занятость
  , atomTempFactor :: Float -- фактор температуры
  , atomElement :: Char -- элемент
  , atomCharge :: Maybe Int -- заряд
  } deriving(Show)

-- | Тип, представляющий из себя CONNECT
--
data PDBBond = PDBBond
  { bondAtom   :: Int    -- номер атома
  , bondAtoms  :: [Int]  -- номера атомов, с которыми связан данный атом
  } deriving(Show)

-- | Тип, представляющий из себя MODEL
--
data PDBModel
  = PDBModel
      { atoms :: [PDBAtom] -- атомы из секции ATOM
      , bonds :: [PDBBond] -- связи из секции CONNECT
      } deriving(Show)

-- | PDB-файл
--
newtype PDB = PDB [PDBModel]

-- 3.a Распарсите `only_atoms.pdb` (2,25 балла)
--     Для выполнения задания фактически нужно научиться парсить только секцию MODEL, 
--     в которой может содержаться только секция ATOM.
modelP :: Parser PDBModel
modelP = PDBModel
  <$> (stringP "MODEL" *> newLineP *> atomsP <* stringP "ENDMDL")
  <*> pure []

atomsP :: Parser [PDBAtom]
atomsP = (:) <$> atomP <*> atomsP <|> ([] <$ stringP "ENDMDL")

atomP :: Parser PDBAtom
atomP = PDBAtom
  <$> (stringP "ATOM" *> spaceP *> intP) -- номер атома
  <*> (spaceP *> symbolsP) -- имя атома
  <*> (spaceP *> symbolsP) -- имя остатка
  <*> (spaceP *> symbolP) -- идентификатор цепи
  <*> (spaceP *> intP) -- последовательность остатков
  <*> ((,,) <$> (spaceP *> signedFloatP) <*> (spaceP *> signedFloatP) <*> (spaceP *> signedFloatP)) -- позиция атома в трехмерном пространстве
  <*> (spaceP *> floatP) -- занятость
  <*> (spaceP *> floatP) -- фактор температуры
  <*> (fst <$> (spaceP *> elementAndChargeP)) -- элемент
  <*> (snd <$> elementAndChargeP) -- заряд
  <* many spaceP

signedFloatP :: Parser Float
signedFloatP = fmap calculate $ (,) <$> optional (satisfyP (== '-')) <*> floatP'
  where
    floatP' = (+) . fromIntegral <$> intP <* satisfyP (== ' ') <*> helper
    helper = foldl' (\ acc x -> 0.1 * (acc + fromIntegral x)) 0.0 . reverse <$> digitsP
    calculate (sign, num) = case sign of
      Just _  -> -num
      Nothing -> num

stringP :: String -> Parser String
stringP str = Parser go
  where
    go :: String -> Maybe (String, String)
    go input
      | str `isPrefixOf` input = Just (str, drop (length str) input)
      | otherwise = Nothing

elementAndChargeP :: Parser (Char, Maybe Int)
elementAndChargeP = (,) <$> symbolP <*> optional signedIntP

signedIntP :: Parser Int
signedIntP = negate <$> (symbolP *> intP <* satisfyP (== '-')) <|> intP

-- 3.b Распарсите `atoms_and_bonds.pdb` (1,25 балл)
--     Придётся научиться парсить секцию CONNECT.

modelAndBondP :: Parser PDBModel
modelAndBondP = PDBModel
  <$> (stringP "MODEL" *> newLineP *> atomsP <* stringP "ENDMDL" <* newLineP)
  <*> many bondP

bondP :: Parser PDBBond
bondP = PDBBond
  <$> (stringP "CONECT" *> spaceP *> intP) -- номер атома
  <*> many (spaceP *> intP) <* newLineP -- номера атомов, с которыми связан данный атом

-------------------------------------------------------------------------------

-- 4. Monad Parser (0,5 балла)
--    (можно без док-ва законов)

-- | Определен в файле Parser.hs
--   newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- перенесла в Parser


-------------------------------------------------------------------------------

-- 5. Реализуйте инстансы Applicative и Monad для нескольких типов (2,75 балла)

--    Покажите выполнение законов класса Applicative и Monad для вашей реализации (Functor не нужно)
--    https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html#t:Monad

--Applicative
-- Identity
--     pure id <*> v = v
-- Composition
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- Homomorphism
--     pure f <*> pure x = pure (f x)
-- Interchange
--     u <*> pure y = pure ($ y) <*> u

-- Monad
-- Left identity
--     return a >>= k = k a
-- Right identity
--     m >>= return = m
-- Associativity
--     m >>= (\x -> k x >>= h) = (m >>= k) >>= h
---------------------------------------

-- 5.a Maybe (0,75 балла)

data Maybe' a = Nothing' | Just' a
  deriving (Show, Eq)

-- Monad зависит от Applicative, Applicative -- от Functor,
-- поэтому нужно реализовывать и эти 2 класса при реализации Monad

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _ Nothing' = Nothing'
  fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure = Just'

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Nothing' <*> _ = Nothing'
  _ <*> Nothing' = Nothing'
  (Just' f) <*> (Just' a) = Just' (f a)

-- Identity
-- pure id <*> Nothing' = Nothing'
-- pure id <*> Just' f = Just' id <*> Just' f = Just' (id f) = Just' f

-- Composition
-- pure (.) <*> Just' u <*> Just' v <*> Just' w = Just' (.) <*> Just' u <*> Just' v <*> Just' w =
-- = Just' ((.) u) <*> Just' v <*> Just' w = Just' ((.u)v) <*> Just' w = Just' (u . v) <*> Just' w =
-- = Just' ((u . v) w) = Just'(u (v w))
-- С другой стороны
-- Just' u <*> (Just' v <*> Just' w) = Just' u <*> Just' (v w) = Just' ( u (v w))
-- Just'(u (v w)) = Just' ( u (v w))

-- Homomorphism
-- pure f <*> pure x = Just' f <*> Just' x = Just' (f x)
-- С другой стороны
-- pure (f x) = Just' (f x)
--  Just' (f x) = Just' (f x)

-- Interchange
-- Just' u <*> pure y = Just' u <*> Just' y = Just' (u y)
-- С другой стороны 
-- pure ($ y) <*> Just' u = Just' ($ y) <*> Just' u = Just' ($ y u) = Just' (u y)
-- Just' (u y) = Just' (u y)

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Nothing' >>= _ = Nothing'
  (Just' a) >>= f = f a

-- Left identity
--     return a >>= k = k a
-- return a >>= k = Just' a >>= k  = k a

-- Right identity
--     m >>= return = m
-- Just' a >>= return = return a = Just' a
-- Nothing' >>= return = return Nothing' = Nothing'

-- Associativity
--     m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- Just' a  >>= (\x -> k x >>= h) = (\x -> k x >>= h) a = | преминяем функцию в скобочка к а| =
-- = k a >>= h
-- С другой стороны
-- (Just' a  >>= k) >>= h = k a >> = h
-- для Nothing 
-- Nothing' >>= (\x -> k x >>= h) = Nothing'
-- С другой стороны
-- (Nothing' >>= k) >>= h = Nothing'
---------------------------------------

-- 5.b Список (1 балл)
--     Подумайте, как нужно матчить списки функций и элементов при реализации <*>:
--     zip или каждый с каждым?
newtype List a = List { getList :: [a] } -- чтобы не ругался на переопределение 

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap f (List xs) = List (map f xs)

instance Applicative List where
  pure :: a -> List a
  pure x = List [x]

  (<*>) :: List (a -> b) -> List a -> List b
  (List fs) <*> (List xs) = List [f x | f <- fs, x <- xs]

--Applicative
-- Identity
--     pure id <*> v = v
-- pure id <*> v = List [id v1, id v2, ..., id vn] = List [v1, v2, ..., vn] = v

-- Composition
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- u = List [u1, u2, ..., un], v = List [v1, v2, ..., vm], w = List [w1, w2, ..., wl]
-- pure (.) <*> u <*> v <*> w = [(.)] <*> u <*> v <*> w = List [(.) u | u <- un] <*> v <*> w =
-- = List [((.) u) v  | u <- un, v <- vm] <*> w =
-- = List [(u . v) | u <- un, v <- vm] <*> w = 
-- = List [(u . v) w | u <- un, v <- vm, w <- wl] = 
-- = List [(u (v w) | u <- un, v <- vm, w <- wl] 
-- С другой стороны
-- u <*> (v <*> w) = u <*> List [v w | v <- vm, w <- wl] = 
-- = List [(u (v w) | u <- un, v <- vm, w <- wl] 

-- Homomorphism
--     pure f <*> pure x = pure (f x)
-- pure f <*> pure x = List[f] <*> List[x] = List [f x | f <- fs, x <- xs]
-- С другой стороны
-- pure (f x) = List [f x | f <- fs, x <- xs]

-- Interchange
--     u <*> pure y = pure ($ y) <*> u
-- u <*> pure y = List [u1, u2, ..., un] <*> List [y1, y2, .., yn] = List [u y | u <- un, y <- yn]
-- С другой стороны
-- pure ($ y) <*> u = List [$y] <*> List[u] = List[$y u] = List [u y | u <- un, y <- yn]

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  (List xs) >>= f = List $ concatMap (getList . f) xs

-- Left identity
--     return a >>= k = k a
-- return a >>= k = List[a] >>= k = |>>= применяет функцию k к каждому элементу списка List [a]| =
-- = k getList(List[a])

-- Right identity
--     m >>= return = m
-- List[a] >>= return  = getList(List[a])

-- Associativity
--     m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- m >>= (\x -> k x >>= h) -  применяем функцию \x -> k x >>= h к элементам списка List[a] 
-- то есть будет List[h k a]
-- С другой стороны (m >>= k) >>= h - потменяем k лщ всем эелементам списка List[a], а затем к ним применяем h
-- то есть будет List[h k a]
---------------------------------------

-- 5.c Either (1 балл)
--     Подумайте, что делать с "экстра" типом-параметром

data Either' a b = Left' a | Right' b
  deriving (Show, Eq)

instance Functor (Either' a) where
  fmap :: (b -> c) -> Either' a b -> Either' a c
  fmap _ (Left' x) = Left' x
  fmap f (Right' y) = Right' (f y)

instance Applicative (Either' a) where
  pure :: b -> Either' a b
  pure = Right'

  (<*>) :: Either' a (b -> c) -> Either' a b -> Either' a c
  Left' x <*> _ = Left' x
  _ <*> Left' x = Left' x
  Right' f <*> Right' y = Right' (f y)

-- Identity
--     pure id <*> v = v
-- Для Left' 
-- pure id <*> Left' x = Right' x <*> Left' x = Left' x
-- Для Right' y
-- pure id <*> Right' y = Right' id <*> Right' y = Right' (id y) = Right' y

-- Composition
--     pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- Для u = Left' x, v = Left' y, w = Left' z
-- pure (.) <*> Left' x <*> Left' y <*> Left' z = Right' (.) <*> Left' x <*> Left' y <*> Left' z = 
-- = Left' x <*> Left' y <*> Left' z = Left' x
-- С другой стороны
-- Left' x <*> (Left' y <*> Left' z) = Left' x <*> Left' y = Left' x
-- Для u = Right' f, v = Right' g, w = Right' h
-- pure (.) <*> Right' f <*> Right' g <*> Right' h = Right' (.) <*> Right' f <*> Right' g <*> Right' h  =
-- = Right' ((.) f)  <*> Right' g <*> Right' h =
-- = Right' ((. f) g)  <*> Right' h = 
-- = Right' (f . g) <*> Right' h =  Right' ((f . g) h) = Right'(f (g h))
-- С другой стороны
-- Right' f <*> (Right' g <*> Right' h) = Right' f <*> Right' (g h) =
-- = Right' (f (g h))

-- Homomorphism
--     pure f <*> pure x = pure (f x)
--  pure f <*> pure x = Right' f <*> Right' x = Right' (f x)
-- С другой стороны
-- pure (f x) = Right' (f x)
-- Interchange
--     u <*> pure y = pure ($ y) <*> u
-- Для u = Left' x
-- Left' x <*> pure y = Left' x <*> Right' y = Left' x 
-- С другой стороны
--  pure ($ y) <*> Left' x = Right' ($ y) <*> Left' x = Left' x 
-- u = Right' x
-- Right' x <*> pure y = Right' x <*> Right' y = Right' (x y)
-- С другой стороны
-- pure ($ y) <*> Right' x = Right' ($ y) <*> Right' x = Right' ($y x) = Right' (x y)

instance Monad (Either' a) where
  (>>=) :: Either' a b -> (b -> Either' a c) -> Either' a c
  Left' x >>= _ = Left' x
  Right' y >>= f = f y

-- Left identity
--     return a >>= k = k a
-- Для Left' 
-- return a >>= Left' = Right' a >>= Left' = Left' a
-- Для Right'
-- return a >>= Right' = Right' a >>= Right' = Right' a

-- Right identity
--     m >>= return = m
-- Для Left' x
-- Left' x >>= return = Left' x >>= Right' =  Left' x
-- Для Right' x
-- Right' x >>= return = Right' x >>= Right'= Right' x

-- Associativity
--     m >>= (\x -> k x >>= h) = (m >>= k) >>= h
-- Для m = Left' x, k = Left' y, h = Left' z
-- Left' x >>= (\x -> Left' y x >>= Left' z) = Left' x 
-- С другой стороны
-- (Left' x >>= Left' y) >>= Left' z = Left' x >>= Left' z =Left' x
-- Для m = Right' x, k = Right' y, h = Right' z
-- Right' x >>= (\x -> Right' y x >>= Right' z) = (\x -> Right' y x >>= Right' z) y = | применяем (\x -> Right' y x >>= Right' z) к y =
-- = Right' x y >>= Right' z = x y z
-- С другой стороны
-- (Right' x >>= Right' y) >>= Right' z = (x y) >>= Right' z  = x y z
-------------------------------------------------------------------------------

-- 6. Что называется "стрелкой Клейсли"? (0,25 балла)
--  Функция, которая принимает обычное значение, а возвращает обернутое
-------------------------------------------------------------------------------
