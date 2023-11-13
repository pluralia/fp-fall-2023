module MyLib where

import           Control.Applicative (Alternative (..))

import           Data.Char           (digitToInt)
import           Data.Foldable       (foldl')
import           Data.List           (singleton)
import           Data.Map.Strict     (Map, fromList)
import qualified Data.Text           as T
import           Parser

-------------------------------------------------------------------------------

-- 1. Простые парсеры (1,25 балла)

---------------------------------------

-- | 1.a Парсит новую строку (0,25 балла)
--
newLineP :: Parser Char
newLineP = satisfyP (== '\n')

---------------------------------------

-- | 1.b Парсит целое число (0,25 балла)
--       (какой парсер из модуля Parser можно переиспользовать?)
--
intP :: Parser Int
intP = foldl' (\ acc x -> acc * 10 + x) 0 <$> digitsP

intP2 :: Parser Int         -- парсит числа формата [1...9][0...9]+
intP2 = foldl' (\ acc x -> acc * 10 + x) 0 <$> ((:) <$> firstDigitP <*> digitsP)
  where
    firstDigitP :: Parser Int
    firstDigitP = digitToInt <$> satisfyP (`elem` ['1'..'9'])

---------------------------------------

-- 1.c Парсит последовательность DNA (0,5 балла)

data DNA = A | T | G | C
  deriving (Eq, Show)

dnaP :: Parser DNA
dnaP = charToDna <$> satisfyP (`elem` "ATGC")
  where
    charToDna :: Char -> DNA
    charToDna s | s == 'A'  = A :: DNA
                | s == 'T'  = T :: DNA
                | s == 'G'  = G :: DNA
                | s == 'C'  = C :: DNA
                | otherwise = error "Uncorrect"

dnaSeqP :: Parser [DNA]
dnaSeqP = some dnaP

---------------------------------------

-- | 1.d Парсит заданную строку (0,25 балла)
--
stringP :: String -> Parser String
stringP str = Parser (helper str)
  where
    helper :: String -> String -> Maybe (String, String)
    helper []     x      = Just (str, x)
    helper _      []     = Nothing
    helper (s:ss) (x:xs) = if s == x then helper ss xs else Nothing

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
  <$> intP
  <* spaceP
  <* satisfyP (== '*')
  <* spaceP
  <*> intP

---------------------------------------

-- 2.b Парсит перемножение вещественных чисел (0,75 балла)

-- | Парсит вещественное число формата `[1..9][0..9]+,[0..9]*` (0,5 балла)
-- + - some, * - many
--
floatP :: Parser Float
floatP = (+) . fromIntegral <$> intP2 <* satisfyP (== ',') <*> helper
  where
    helper :: Parser Float
    helper = foldl' (\ acc x -> 0.1 * (acc + fromIntegral x)) 0.0 . reverse <$> digitsP

-- | Парсит 2 вещественных числа и перемножает их (0,25 балла)
-- 
multFloatsP :: Parser Float
multFloatsP = (*) <$> floatP <* spaceP <* satisfyP (== '*') <* spaceP <*> floatP

sumFloatsP :: Parser Float
sumFloatsP = (+) <$> floatP <* spaceP <* satisfyP (== '+') <* spaceP <*> floatP

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
simpleExprP = SimpleExpr <$> floatP <* spaceP <*> satisfyP (`elem` "*+") <* spaceP <*> floatP

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
exprP = Expr <$> floatP <* spaceP <*> takeOperation <* spaceP <*> floatP
  where
    takeOperation :: Parser Operation
    takeOperation = Mult <$ satisfyP (== '*') <|> Sum <$ satisfyP (== '+')

-- | Реализуйте парсер, который парсит перемножение/сложение 2 вещественных чисел.
--   Используйте exprP (0,25 балла)
--
sumMultFloatsP :: Parser Float
sumMultFloatsP = operat <$> exprP
  where
    operat :: Expr -> Float
    operat Expr {left=l, op=Mult, right=r} = l * r
    operat Expr {left=l, op=Sum,  right=r} = l + r

-------------------------------------------------------------------------------

-- 3. fmap<n>, <$><*> и парсер-комбинаторы (2,25 балла)

---------------------------------------

-- 3.a | Реализуйте fmap4 через fmap -- прокомментируйте каждый шаг указанием типов (0,5 балла)
--
fmap4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
fmap4 f fa fb fc fd = f <$> fa <*> fb <*> fc <*> fd

-- (<*>) ::  f (x -> y) -> f x -> f y
-- (<$>) ::    (x -> y) -> f x -> f y

-- fmap :: (x -> y) -> f x -> f y
-- fa :: f a
-- fb :: f b
-- fc :: f c
-- fd :: f d
-- res :: f e

-- f :: (a -> b -> c -> d -> e) = (a -> (b -> c -> d -> e))
-- x = a, y = b -> c -> d -> e
-- fmap :: (a -> (b -> c -> d -> e)) -> f a -> f (b -> c -> d -> e)
-- fmap f :: f a -> f (b -> c -> d -> e)
-- ffa = f <$> fa :: f (b -> c -> d -> e)

-- x = b, y = c -> d -> e
-- ffa :: f (b -> c -> d -> e)
-- ffa <*> :: f b -> f (c -> d -> e)
-- ffafb = ffa <*> fb :: f (c -> d -> e)

-- x = c, y = d -> e
-- ffafb :: f (c -> d -> e)
-- ffafb <*> :: f c -> f (d -> e)
-- ffafbfc = ffafb <*> fc :: f (d -> e)

-- x = d, y = e
-- ffafbfc :: f (d -> e)
-- ffafbfc <*> :: f d -> f e
-- res = ffafbfc <*> fd :: f e

---------------------------------------

-- 3.b Сравните реализацию fmap4 и multDigitsP/simpleExprP/exprP -- видите похожий паттерн?
--     Поделитесь своими мыслями / наблюдениями на этот счет (0,25 балла)

-- мы работаем над чем-то __в контексте__: в случае парсеров "обёрткой" выступает Maybe и Parser,
-- а в случае fmap4 обёрткой может служить любой контекст 'f' 
-- так что и там, и там мы преобразовываем что-то, завёрнутое в контекст, поэтому их реализации так похожи.

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

-- Подсказка: пусть (<$>) :: (a -> b) -> f a -> f b;  f для gcContent -- это (-> T.Text)

-- a = Double
-- b = Double -> Double
-- f = T.text ->

-- (<$>) ::    (x -> y) -> f x -> f y
-- (<*>) ::  f (x -> y) -> f x -> f y

-- <$> :: (Double -> Double)
-- (a      -> b                 ) -> f          a       -> f          b
-- (Double -> (Double -> Double)) -> (T.Text -> Double) -> (T.Text -> (Double -> Double))
--                                   первый len            результат применения <$> = res

-- <*>  :: f          (x      -> y)       -> f x                  -> f y
-- <*>  :: (T.Text -> (Double -> Double)) -> (T.Text -> Double)   -> (T.Text -> Double)
--          отправляем res сюда              применяем второй len -> gcContent -- наша функция!

-------------------------------------------------------------------------------

-- 4. Более сложные парсеры (2,5 балла)

---------------------------------------

-- | 4.a Парсит весь поток, пока символы потока удовлетворяют заданному условию (0,5 балла)
--
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP = many . satisfyP

---------------------------------------

-- | 4.b Парсер, который падает с ошибкой, если в потоке что-то осталось.
--       В противном случае парсер отрабатывает успешно (0,5 балла)
--
eofP :: Parser ()
eofP = Parser go
  where
    go :: String -> Maybe ((), String)
    go s | null s    = Just ((), s)
         | otherwise = Nothing

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
listP p = inBetweenP "[" "]" (moreElemP p <|> zeroElemP p)
  where
    zeroElemP :: Parser a -> Parser [a]
    zeroElemP _ = [] <$ spaceP
    moreElemP :: Parser a -> Parser [a]
    moreElemP p' = (:) 
      <$ spaceP 
      <*> p' 
      <*> many (spaceP 
                *> satisfyP (== ',') 
                *> spaceP 
                *> p')

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
intOrStringP :: Parser (Either [Int] String)
intOrStringP = Left <$> digitsP
  <|> Right <$> symbolsP

-- | Напишите парсер для 'Value' (0,5 балла)
--
valueP :: Parser Value
valueP = FloatValue <$> floatP <|> IntValue <$> intP <|> StringValue <$> symbolsP

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
abstractRowP sep p = moreElemP p <|> zeroElemP p
  where
    zeroElemP :: Parser a -> Parser [a]
    zeroElemP _ = [] <$ spaceP
    moreElemP :: Parser a -> Parser [a]
    moreElemP p' = (:) 
      <$ spaceP 
      <*> p' 
      <*> many (spaceP 
                *> satisfyP (==sep) 
                *> spaceP 
                *> p')

---------------------------------------

-- | 6.b Реализуйте парсер, который парсит 'Row'. 
--       Названия колонок файла передаются аргументом (0,5 балла)
--
-- по умолчанию считаем, что разделитель ',' как в стандартных csv-файлах
rowP :: [String] -> Parser Row
rowP colnames = Row . fromList . zip colnames <$> abstractRowP ',' valueP

-------------------------------------------------------------------------------
