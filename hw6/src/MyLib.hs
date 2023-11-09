module MyLib where

import           Control.Applicative (Alternative (..))
import           Data.Foldable       ()
import           Data.Map.Strict     (Map, fromList)
import qualified Data.Text           as T
import           Parser
import           Data.List
-------------------------------------------------------------------------------

-- 1. Простые парсеры (1,25 балла)

---------------------------------------

-- | 1.a Парсит новую строку (0,25 балла)
--
newLineP :: Parser Char
newLineP = satisfyP (=='\n')

---------------------------------------

-- | 1.b Парсит целое число (0,25 балла)
--       (какой парсер из модуля Parser можно переиспользовать?)
--
intP :: Parser Int
intP = fmap digitsToInt digitsP
  where
    digitsToInt = foldl' (\acc d -> acc * 10 + d) 0
---------------------------------------

-- 1.c Парсит последовательность DNA (0,5 балла)

data DNA = A | T | G | C
  deriving (Eq, Show)

dnaP :: Parser DNA
dnaP = aP <|> tP <|> gP <|> cP
  where
    aP = A <$ satisfyP (=='A')
    tP = T <$ satisfyP (=='T')
    gP = G <$ satisfyP (=='G')
    cP = C <$ satisfyP (=='C')

dnaSeqP :: Parser [DNA]
dnaSeqP = many dnaP

---------------------------------------

-- | 1.d Парсит заданную строку (0,25 балла)
--
stringP :: String -> Parser String
stringP str = Parser go
  where
    go :: String -> Maybe (String, String)
    go input
      | str `isPrefixOf` input = Just (str, drop (length str) input)
      | otherwise = Nothing

-------------------------------------------------------------------------------

-- 2. Парсер-комбинаторы и арифметика (2,5 балла)

-- | Парсеры можно комбинировать
--
multDigitsP :: Parser Int
multDigitsP = (*)        -- перемножаем то, что распарсили
  <$> digitP             -- парсим цифру
  <* spaceP              -- парсим произвольное число пробелов (и не сохраняем их! используя <*)
  <* satisfyP (== '*')   -- парсим символ '*' (и не сохраняем его)
  <* spaceP              -- парсим произвольное число пробелов (и не сохраняем их)
  <*> digitP             -- парсим цифру

runMultDigitsP :: Maybe (Int, String)
runMultDigitsP = runParser multDigitsP "3 * 6" -- Just (18, "")

---------------------------------------

-- 2.a Парсит перемножение целых чисел (0,5 балла)

-- | Парсер multDigitsP умеет работать только с цифрами, но не с числами
--
runMultIntsP' :: Maybe (Int, String)
runMultIntsP' = runParser multDigitsP "33 * 6" -- Nothing

-- | Реализуйте парсер, который умеет парсить 2 числа и перемножать их
-- 
multIntsP :: Parser Int
multIntsP = (*)
  <$> intP -- реализован в 1 b - парсим число
  <* spaceP -- парсим пробелы
  <* satisfyP (== '*') --парсим символ '*'
  <* spaceP -- парсим пробелы
  <*> intP -- реализован в 1 b - парсим число

---------------------------------------

-- 2.b Парсит перемножение вещественных чисел (0,75 балла)

-- | Парсит вещественное число формата `[1..9][0..9]+,[0..9]*` (0,5 балла)
--
floatP :: Parser Float
floatP = read . concat <$> sequenceA [intPart, pure ".", fracPart]
  where
    intPart = concatMap show <$> digitsP
    fracPart = satisfyP (== ',') *> (concatMap show <$> digitsP)

-- | Парсит 2 вещественных числа и перемножает их (0,25 балла)
-- 
multFloatsP :: Parser Float
multFloatsP = (*)
  <$> floatP -- Парсит вещественное число формата `[1..9][0..9]+,[0..9]
  <* spaceP
  <* satisfyP (== '*')
  <* spaceP 
  <*> floatP

---------------------------------------

-- 2.c Парсит перемножение/сложение 2 вещественных чисел (1,25 балл)

-- | Зададим простейшее выражение:
--   левый и правый операнд -- Float, операция -- Сhar
--
data SimpleExpr = SimpleExpr Float Char Float
  deriving (Show, Eq)

-- | Реализуйте парсер для SimpleExpr; операция может быть '+' и '*' (0,25 балла)
--
simpleExprP :: Parser SimpleExpr
simpleExprP = 
  SimpleExpr 
  <$> (spaceP *> floatP <* spaceP) 
  <*> operatorP 
  <*> (spaceP *> floatP <* spaceP)

operatorP :: Parser Char
operatorP = satisfyP (`elem` ['+', '*'])

-- | Более сложное выражение, в котором операция задана типом данных Operation
--
data Expr = Expr {
    left  :: Float
  , op    :: Operation
  , right :: Float
  } deriving (Show, Eq)

data Operation = Mult | Sum
  deriving (Show, Eq)

-- | Реализуйте парсер для Expr (0,75 балла)
--
exprP :: Parser Expr
exprP = 
  Expr 
  <$> (spaceP *> floatP <* spaceP) 
  <*> operationP 
  <*> (spaceP *> floatP <* spaceP)

operationP :: Parser Operation
operationP = f <$> operatorP
  where
    f '+' = Sum
    f '*' = Mult
    f _   = error "Invalid operator"

-- | Реализуйте парсер, который парсит перемножение/сложение 2 вещественных чисел.
--   Используйте exprP (0,25 балла)
--
sumMultFloatsP :: Parser Float
sumMultFloatsP = calculate <$> exprP
  where
    calculate (Expr l Sum r) = l + r
    calculate (Expr l Mult r) = l * r

-------------------------------------------------------------------------------

-- 3. fmap<n>, <$><*> и парсер-комбинаторы (2,25 балла)

---------------------------------------

-- 3.a | Реализуйте fmap4 через fmap -- прокомментируйте каждый шаг указанием типов (0,5 балла)
--
fmap4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
fmap4  f fa fb fc fd = f <$> fa <*> fb <*> fc <*> fd

-- fmap :: (x -> y) -> f x -> f y
-- f :: (a -> b -> c -> d -> e) = (a -> (b -> c -> d -> e))
-- x = a, y = b -> c -> d -> e
-- fmap :: (a -> (b -> c -> d -> e)) -> f a -> f (b -> c -> d -> e)
-- fmap f :: f a -> f (b -> c -> d -> e)
-- ffa = f <$> fa :: f (b -> c -> d -> e)

-- ffa :: f (b -> c -> d -> e)
-- fb :: f b
-- fc :: f c
-- fd :: f d
-- res :: f e

-- (<*>) ::  f (x -> y) -> f x -> f y
-- x = b, y = c -> d -> e
-- (<*>) ::  f (b -> (c -> d -> e)) -> f b -> f (c -> d -> e)
-- ffa <*> :: f b -> f (c -> d -> e)
-- ffafb = ffa <*> fb :: f (c -> d -> e)

-- ffafb :: f (c -> d -> e)
-- fc :: f c
-- fd :: f d
-- res :: f e

-- (<*>) ::  f (x -> y) -> f x -> f y
-- x = c, y =  d -> e
-- (<*>) ::  f (c -> (d -> e)) -> f c -> f (d -> e)
-- ffafb <*> :: f c -> f (d -> e)
-- ffafbfc = ffafb <*> fc :: f (d -> e)

-- ffafbfc :: f (d -> e)
-- fd :: f d
-- res :: f e
-- (<*>) ::  f (x -> y) -> f x -> f y
-- x = d, y = e
---------------------------------------

-- 3.b Сравните реализацию fmap4 и multDigitsP/simpleExprP/exprP -- видите похожий паттерн?
--     Поделитесь своими мыслями / наблюдениями на этот счет (0,25 балла)

-- что мы выполняем друг за другом операции, оставаясь в одном контексте 
---------------------------------------

-- 3.c* Почему код функции `gcContent` работает (1,5 балла)

-- В дз 4 было следующее задание:
-- """
-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.
-- """

gcContent :: T.Text -> Double
gcContent = (/)
  <$> fromIntegral . T.length . T.filter (\c -> c == 'G' || c == 'C')
  <*> fromIntegral . T.length

-- Распишите типы в этой функции на каждом шаге и поймите, почему это работает.
-- Как расписывать типы (собственно, аналогично тому, как мы делали это для fmap2/3):
-- 1. Напишите тип <$>
-- 2. Напишите тип <*> -- используйте типовые переменные, отличные от тех, что вы использовали в п.1
-- 3. Начните подставлять типы функции gcContent в типы <$>
-- 4. Сопоставьте типы <$> и <*>

-- Подсказка: пусть (<$>) :: (a -> b) -> f a -> f b; f для gcContent -- это (-> T.Text)

-------------------------------------------------------------------------------

-- 4. Более сложные парсеры (2,5 балла)

---------------------------------------

-- | 4.a Парсит весь поток, пока символы потока удовлетворяют заданному условию (0,5 балла)
--
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP p = many (satisfyP p)

---------------------------------------

-- | 4.b Парсер, который падает с ошибкой, если в потоке что-то осталось.
--       В противном случае парсер отрабатывает успешно (0,5 балла)
--
eofP :: Parser ()
eofP = Parser f
  where
    f :: String -> Maybe ((), String)
    f [] = Just ((), [])
    f _  = Nothing -- Nothing показатель ошибки

---------------------------------------

-- | 4.c Парсер, который парсит символ @lBorder@, всё, что парсит переданный парсер @p@,
--        а потом — правый символ @rBorder@. Возвращает то, что напарсил парсер @p@ (0,5 балла)
--
inBetweenP :: String -> String -> Parser a -> Parser a
inBetweenP lBorder rBorder p = stringP lBorder *> p <* stringP rBorder

---------------------------------------

-- | 4.d Реализуйте функцию, которая парсит списки вида "[1, a   , bbb , 23     , -7]".
--       Функция принимает парсер, которым парсятся элементы списка (1 балл)
--
listP :: Parser a -> Parser [a]
listP p = inBetweenP "[" "]" (sepBy p commaSep)

commaP :: Parser String
commaP = fmap pure (satisfyP (== ','))

commaSep :: Parser String
commaSep = spaceP *> commaP <* spaceP

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy' p sep <|> pure []

sepBy' :: Parser a -> Parser b -> Parser [a]
sepBy' p sep = (:) <$> p <*> many (sep *> p)
-------------------------------------------------------------------------------

-- 5. Парсим Value (0,5 баллa)

-- | Тип, представляющий значение, которое может храниться в списке.
--
data Value
  = IntValue Int
  | FloatValue Float
  | StringValue String
  deriving (Eq, Show)

-- Значения 'Value' получаются из текста по таким правилам:
-- a) Целые числа превращаются в IntValue.
-- б) Вещественные числа превращаются в FloatValue.
-- в) Всё остальное превращается в StringValue.

-- Парсинг 'Value', по сути, требует трех независимых парсеров:
-- 1) intValueP
-- 2) floatValueP
-- 3) stringValueP

-- Однако, мы не знаем, какой из них нам пригодится в моменте, поэтому хотим по порядку попробовать их все так,
-- чтобы, если первый парсер зафейлился, мы бы попробовали второй и третий.
-- Для объединения парсеров таким образом используйте оператор <|> из класса типов Alternative.

-- | Например, вот так можно распарсить [int] или string
--
intOrFloatP :: Parser (Either [Int] String)
intOrFloatP = Left <$> digitsP
  <|> Right <$> symbolsP

-- | Напишите парсер для 'Value' (0,5 балла)
--
valueP :: Parser Value
valueP = floatValueP <|> intValueP <|> stringValueP

-- Парсер для IntValue
intValueP :: Parser Value
intValueP = IntValue <$> intP

-- Парсер для FloatValue
floatValueP :: Parser Value
floatValueP = FloatValue <$> floatP

-- Парсер для StringValue
stringValueP :: Parser Value
stringValueP = StringValue <$> symbolsP

-------------------------------------------------------------------------------

-- 6. Парсим строку CSV формата (1 балл)

-- Самый популярный формат для хранения табличных данных -- CSV (https://en.wikipedia.org/wiki/Comma-separated_values).

-- | CSV представляет собой список названий колонок и список строк со значениями.
--
data CSV = CSV {
    colNames :: [String] -- названия колонок в файле
  , rows     :: [Row]    -- список строк со значениями из файла
  }

-- | Строка CSV представляет собой отображение названий колонок
--   в их значения в данной строке. Будем считать, что в каждой колонке
--   в данной строке гарантировано есть значение
--
newtype Row = Row (Map String Value)
  deriving (Show, Eq)

---------------------------------------

-- | 6.a Реализуйте парсер для строки из значений, которые могут парситься заданным парсером @p@
--       и разделённых заданным разделителем @sep@ (0,5 балла)
--
abstractRowP :: Char -> Parser a -> Parser [a]
abstractRowP sep p = p `sepBy` satisfyP (== sep)

---------------------------------------

-- | 6.b Реализуйте парсер, который парсит 'Row'. 
--       Названия колонок файла передаются аргументом (0,5 балла)
--
rowP :: [String] -> Parser Row
rowP colNames' = Row . fromList . zip colNames' <$> abstractRowP ',' valueP

-------------------------------------------------------------------------------
