{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module MyLib where

import           Control.Applicative (Alternative (..))
import           Data.Map.Strict     (Map, fromList)
import qualified Data.Text           as T
import           Data.Foldable       (foldl')
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
intP = fmap listToNumber digitsP
  where
    listToNumber :: [Int] -> Int
    listToNumber = foldl' (\acc x -> acc * 10 + x) 0 -- конечно тут должен быть foldl', потому что он эффективнее,
                                                     -- а ленивость тут не пригодится 

-- ---------------------------------------

-- -- 1.c Парсит последовательность DNA (0,5 балла)

data DNA = A | T | G | C
  deriving (Eq, Show)

-- -- Парсер для одного символа DNA (A, T, G или C)
dnaP :: Parser DNA
dnaP = 
      A <$ satisfyP (== 'A')
  <|> T <$ satisfyP (== 'T')
  <|> G <$ satisfyP (== 'G')
  <|> C <$ satisfyP (== 'C')

    

-- -- Парсер для последовательности DNA
dnaSeqP :: Parser [DNA]
dnaSeqP = many dnaP
-- ---------------------------------------

-- -- | 1.d Парсит заданную строку (0,25 балла)
-- --
stringP :: String -> Parser String
stringP "" = pure ""                                    -- Если строка пуста, вернем пустую строку
stringP (x:xs) = (:) <$> satisfyP (== x) <*> stringP xs

-- -------------------------------------------------------------------------------

-- -- 2. Парсер-комбинаторы и арифметика (2,5 балла)

-- -- | Парсеры можно комбинировать
-- --
multDigitsP :: Parser Int
multDigitsP = (*)        -- перемножаем то, что распарсили
  <$> digitP             -- парсим цифру
  <* spaceP              -- парсим произвольное число пробелов (и не сохраняем их! используя <*)
  <* satisfyP (== '*')   -- парсим символ '*' (и не сохраняем его)
  <* spaceP              -- парсим произвольное число пробелов (и не сохраняем их)
  <*> digitP             -- парсим цифру

runMultDigitsP :: Maybe (Int, String)
runMultDigitsP = runParser multDigitsP "3 * 6" -- Just (18, "")

-- ---------------------------------------

-- -- 2.a Парсит перемножение целых чисел (0,5 балла)

-- -- | Парсер multDigitsP умеет работать только с цифрами, но не с числами
-- --
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

-- ---------------------------------------

-- -- 2.b Парсит перемножение вещественных чисел (0,75 балла)

-- -- | Парсит вещественное число формата `[1..9][0..9]+,[0..9]*` (0,5 балла)
-- --
floatP :: Parser Float
floatP = toFloat
  <$> intP
  <* satisfyP (== ',')
  <*> intP
  where
    toFloat :: Int -> Int -> Float
    toFloat integerPart fractionalPart = fromIntegral integerPart + (fromIntegral fractionalPart / (10 ^ length (show fractionalPart)))

-- эту функцию тоже перепишем за компанию

-- -- | Парсит 2 вещественных числа и перемножает их (0,25 балла)
-- -- 
multFloatsP :: Parser Float
multFloatsP = (*)
  <$> floatP  
  <* spaceP
  <* satisfyP (== '*')     
  <* spaceP
  <*> floatP 


-- ---------------------------------------

-- -- 2.c Парсит перемножение/сложение 2 вещественных чисел (1,25 балл)

-- -- | Зададим простейшее выражение:
-- --   левый и правый операнд -- Float, операция -- Сhar
-- --
data SimpleExpr = SimpleExpr Float Char Float
  deriving (Show, Eq)

-- -- | Реализуйте парсер для SimpleExpr; операция может быть '+' и '*' (0,25 балла)
-- --
-- -- Парсер для SimpleExpr
simpleExprP :: Parser SimpleExpr
simpleExprP = SimpleExpr
  <$> floatP
  <* spaceP
  <*> satisfyP (\c -> c == '+' || c == '*')
  <* spaceP
  <*> floatP

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
exprP = Expr
  <$> floatP
  <* spaceP
  <*> operationP
  <* spaceP
  <*> floatP
    where
      operationP :: Parser Operation
      operationP = 
        (Sum <$ satisfyP (== '+')) <|> (Mult <$ satisfyP (== '*'))

-- | Реализуйте парсер, который парсит перемножение/сложение 2 вещественных чисел.
--   Используйте exprP (0,25 балла)
--
sumMultFloatsP :: Parser Float
sumMultFloatsP = (\expr -> case op expr of
    Sum  -> left expr + right expr
    Mult -> left expr * right expr
  ) <$> exprP

-------------------------------------------------------------------------------

-- 3. fmap<n>, <$><*> и парсер-комбинаторы (2,25 балла)

---------------------------------------

-- 3.a | Реализуйте fmap4 через fmap -- прокомментируйте каждый шаг указанием типов (0,5 балла)
--
fmap4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
fmap4 f fa fb fc fe = f <$> fa <*> fb <*> fc <*> fe

-- <$> == fmap :: (x -> y) -> f x -> f y 
-- f :: (a -> b -> c -> d -> e) = (a -> (b -> c -> d -> e))
-- x = a, y = b -> c -> d -> e

-- fmap f :: f a -> f (b -> c -> d -> e)
-- ffa = f <$> fa :: f (b -> c -> d -> e)

-- ffa :: f (b -> c -> d -> e)
-- ffafb :: f (c -> d -> e)
-- ffafbfc :: f (d -> e)

-- (<*>) ::  f (x -> y) -> f x -> f y
-- x = a, y = b -> c -> d -> e
-- (<*>) ::  f (b -> (c -> d -> e)) -> f b -> f (c -> d -> e)
-- ffa <*> :: f b -> f (c -> d -> e)
-- ffafb = ffa <*> fb :: f (c -> d -> e)
-- ffafbfc = ffa <*> fb <*> :: f (d -> e)

-- res :: f e
-- (<*>) ::  f (x -> y) -> f x -> f y
-- x = d, y = e

---------------------------------------

-- 3.b Сравните реализацию fmap4 и multDigitsP/simpleExprP/exprP -- видите похожий паттерн?
--     Поделитесь своими мыслями / наблюдениями на этот счет (0,25 балла)
-- Давайте рассмотрим на примере функции simpleExprP
-- Ее можно переписать так:
-- simpleExprP :: Parser SimpleExpr
-- simpleExprP = SimpleExpr <$> floatP <* spaceP <*> satisfyP (\c -> c == '+' || c == '*') <* spaceP <*> floatP

-- Или так
-- simpleExprP :: Parser SimpleExpr
-- simpleExprP = f <$> fa <* fb <*> fc <* fd <*> fe
  -- where
  --   f  = SimpleExpr
  --   fa = floatP
  --   fb = spaceP
  --   fc = satisfyP (\c -> c == '+' || c == '*')
  --   fd = spaceP
  --   fe = floatP

-- Видим практически идентичный паттерн для simpleExprP и fmap4 (потому что и там и там мы находимся в контексте), с той лишь разницей,
-- что мы не хотим запоминать пробелы и операнды и используем <*, а не <*>.
--
-- Такая конструкция оказывается очень удобной в контексте парсеров. 
-- Она улучшает читабельность функций и упрощает их написание.

-- В данном случае, fmap принимает парсер aP и функцию g,
-- и возвращает новый парсер, который применяет функцию g к результату парсера aP.
-- В данном контексте, f будет функцией, которая принимает строку ввода и возвращает пару (b, String),
-- где b - это результат применения функции g к результату парсера aP,
-- и String - оставшаяся часть строки ввода после парсинга.
-- 
-- Тогда вот как выглядит тип f в определении fmap для Parser:
-- f :: String -> Maybe (b, String)
-- Этот тип подразумевает, что f принимает строку ввода и возвращает Maybe,
-- где Just содержит пару с результатом парсинга типа b и оставшейся частью строки ввода, или Nothing, если парсинг не удался.
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
-- <$> == fmap :: (x -> y) -> f x -> f y 
-- (<*>) ::  f (x -> y) -> f x -> f y

-- f = (T.Text ->) (тут я не понял почему в подсказке f это (-> T.Text),
--                  потому что в gcContent мы ничего в T.Text не преобразовывали, наоборот из него в Double)
-- a = Double
-- b = Double -> Double

-- С учетом последнего перепишем:
-- <$> == fmap :: (Double -> (Double -> Double)) -> (T.Text -> Double) -> (T.Text -> (Double -> Double)) - написал неправильный тип для b
--                                                  --левый T.length--     --результат <$>--
-- (<*>) ::  f (T.Text -> (Double -> Double)) -> (T.Text -> Double) -> (T.Text -> Double)
               --сюда отправляем результат <$>--   -- правый T.length--  --получили gcContent--

-------------------------------------------------------------------------------

-- 4. Более сложные парсеры (2,5 балла)

---------------------------------------

-- | 4.a Парсит весь поток, пока символы потока удовлетворяют заданному условию (0,5 балла)
--
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP = many . satisfyP

-- takeWhileP :: (Char -> Bool) -> Parser String
-- takeWhileP condition = many (satisfy condition)

-- ---------------------------------------

-- -- | 4.b Парсер, который падает с ошибкой, если в потоке что-то осталось.
-- --       В противном случае парсер отрабатывает успешно (0,5 балла)
-- --
eofP :: Parser ()
eofP = Parser go
  where
    go :: String -> Maybe ((), String)
    go input
      | null input = Just ((), input)  -- Если входной поток пуст, успешно завершаем парсинг
      | otherwise = Nothing            -- В противном случае выбрасываем ошибку

-- ---------------------------------------

-- -- | 4.c Парсер, который парсит символ @lBorder@, всё, что парсит переданный парсер @p@,
-- --        а потом — правый символ @rBorder@. Возвращает то, что напарсил парсер @p@ (0,5 балла)
-- --
inBetweenP :: String -> String -> Parser a -> Parser a
inBetweenP lBorder rBorder p = stringP lBorder *> p <* stringP rBorder

-- ---------------------------------------

-- -- | 4.d Реализуйте функцию, которая парсит списки вида "[1, a   , bbb , 23     , -7]".
-- --       Функция принимает парсер, которым парсятся элементы списка (1 балл)
-- --
listP :: Parser a -> Parser [a]
listP elP = inBetweenP "[" "]" (moreElementsParser elP <|> zeroElementsParser)
  where
    -- Парсер для строки с несколькими элементами
    moreElementsParser :: Parser a -> Parser [a]
    moreElementsParser p'' = (:) <$> p'' <*> many (spaceP *> satisfyP (== ',') *> spaceP *> p'')
    
    -- Парсер для строки без элементов
    zeroElementsParser :: Parser [a]
    zeroElementsParser     = pure []

-- так правда лучше, бонусом добавил типы 



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
intOrFloatP = Left  <$> digitsP
          <|> Right <$> symbolsP

-- -- | Напишите парсер для 'Value' (0,5 балла)
-- --
valueP :: Parser Value
valueP = FloatValue  <$> floatP   -- важно сначала обработать вещественные числа,
     <|> IntValue    <$> intP     -- а уже после этого целые,
     <|> StringValue <$> symbolsP -- иначе функция отработает некорректно


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
abstractRowP sep p = moreElementsParser p <|> zeroElementsParser
  where
    -- Парсер для строки с несколькими элементами
    moreElementsParser :: Parser a -> Parser [a]
    moreElementsParser p' = (:) <$> (spaceP *> p' <* spaceP)
                                <*> many (spaceP
                                          *> satisfyP (== sep)
                                          *> spaceP
                                          *> p'
                                          <* spaceP)
    
    -- Парсер для строки без элементов
    zeroElementsParser :: Parser [a]
    zeroElementsParser = pure []

---------------------------------------

-- | 6.b Реализуйте парсер, который парсит 'Row'. 
--       Названия колонок файла передаются аргументом (0,5 балла)
--
rowP :: [String] -> Parser Row
rowP colN = Row . fromList . zip colN <$> abstractRowP ',' valueP -- сейчас понял, что это решение не будет работать для float,
                                                                    -- потому что определенный для них разделитель ',' совпадает с разделителем csv файла,
                                                                    -- можно заменить разделитель на точку в функции floatP, тогда все заработает
-------------------------------------------------------------------------------
