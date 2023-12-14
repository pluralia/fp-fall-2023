{-# LANGUAGE LambdaCase #-}

module MyLib where

import           Control.Applicative (Alternative (..), optional)
import           Data.Foldable       (foldl')
import           Data.Map.Strict     (Map, fromList)
import           Data.Maybe ( fromMaybe )
import qualified Data.Text           as T
import Parser
-------------------------------------------------------------------------------

-- В этой домашке вам потребуется подгружать одновременно 2 файла в ghci:
-- src/Parser.hs и src/MyLib.hs. Это требует 2 шага:
--
-- ghci> :l src/Parser.hs src/MyLib.hs
-- ghci> :m Parser MyLib

-------------------------------------------------------------------------------

-- 1. Простые парсеры (1,75 балл)

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
intP = (*) <$> signMultiplier <*> (foldl' (\acc d -> acc * 10 + d) 0 <$> digitsP)
  where
    signMultiplier = (\s -> if s == Just '-' then (-1) else 1) <$> optional (satisfyP (== '-'))

---------------------------------------

-- | 1.c Парсит вещественное число формата `-?(0|[1-9]\d*).\d*` (0,75 балла)
--
floatP :: Parser Float
floatP = (\sign intPart fracPart -> sign * (fromIntegral intPart + fracPart)) 
          <$> signMultiplier <*> intP2 <*> helper
  where
    signMultiplier = (\s -> if s == Just '-' then (-1) else 1) 
                      <$> optional (satisfyP (== '-'))
    
    intP2 :: Parser Int
    intP2 = foldl' (\acc d -> acc * 10 + d) 0 <$> digitsP
    
    helper :: Parser Float
    helper = foldr (\x acc -> (acc + fromIntegral x) / 10) 0 <$> (satisfyP (== '.') *> some digitP)
---------------------------------------

-- | 1.d Парсит заданную строку (0,25 балла)
--
stringP :: String -> Parser String
stringP = foldr (\ x -> (<*>) ((:) <$> satisfyP (== x))) (pure [])

---------------------------------------

-- 1.e Парсит Value (0,25 баллa)

data Value = IntValue Int | FloatValue Float | StringValue String
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

-- | Напишите парсер для 'Value'
--
intValueP :: Parser Value
intValueP = IntValue <$> intP

floatValueP :: Parser Value
floatValueP = FloatValue <$> floatP

stringValueP :: Parser Value
stringValueP = StringValue <$> symbolsP

valueP :: Parser Value
valueP = floatValueP <|> intValueP <|> stringValueP
-------------------------------------------------------------------------------

-- 2. Парсер-комбинаторы и арифметика (1 балла)

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

-- 2.a Парсит перемножение чисел (0,5 балла)

-- | Парсер multDigitsP умеет работать только с цифрами, но не с числами
--
runMultIntsP' :: Maybe (Int, String)
runMultIntsP' = runParser multDigitsP "33 * 6" -- Nothing

-- | Реализуйте парсер, который умеет парсить 2 целых числа и перемножать их
-- 
multIntsP :: Parser Int
multIntsP = (*) <$> intP <* spaceP <* satisfyP (== '*') <* spaceP <*> intP

-- | Парсит 2 вещественных числа и перемножает их
-- 
multFloatsP :: Parser Float
multFloatsP = (*) <$> floatP <* spaceP <* satisfyP (== '*') <* spaceP <*> floatP

---------------------------------------
-- 2.b Парсит перемножение/сложение 2 вещественных чисел (0,5 балла)

-- | Зададим простейшее выражение:
--   левый и правый операнд -- Float, операция -- Сhar
--
data SimpleExpr = SimpleExpr Float Char Float
  deriving (Show, Eq)

-- | Реализуйте парсер для SimpleExpr; операция может быть '+' и '*'
--
simpleExprP :: Parser SimpleExpr
simpleExprP = SimpleExpr
  <$> floatP
  <*> satisfyP (\c -> c == '+' || c == '*')
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

-- | Реализуйте парсер для Expr
--
operationP :: Parser Operation
operationP = (Mult <$ satisfyP (== '*')) <|> (Sum <$ satisfyP (== '+'))

exprP :: Parser Expr
exprP = Expr
  <$> floatP
  <*> operationP
  <*> floatP

-- | Реализуйте парсер, который парсит перемножение/сложение 2 вещественных чисел.
--   Используйте exprP
--
sumMultFloatsP :: Parser Float
sumMultFloatsP = (\expr -> case op expr of
                    Mult -> left expr * right expr
                    Sum  -> left expr + right expr)
                    <$> exprP

-------------------------------------------------------------------------------

-- 3. fmap<n>, <$><*> и парсер-комбинаторы (0,75 балла)

---------------------------------------

-- 3.a | Реализуйте fmap4 через fmap -- прокомментируйте каждый шаг указанием типов (0,5 балла)
--
fmap4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f c
fmap4 = undefined

---------------------------------------

-- 3.b Сравните реализацию fmap4 и multDigitsP/simpleExprP/exprP -- видите похожий паттерн?
--     Поделитесь своими мыслями / наблюдениями на этот счет (0,25 балла)

-------------------------------------------------------------------------------

-- 4. Более сложные парсеры (2,25 балла)

---------------------------------------

-- | 4.a Парсит весь поток, пока символы потока удовлетворяют заданному условию (0,25 балла)
--
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP condition = Parser $ \input ->
  let (matched, remaining) = span condition input
  in Just (matched, remaining)

---------------------------------------

-- | 4.b Парсер, который фейлится, если в потоке что-то осталось.
--       В противном случае парсер отрабатывает успешно (0,5 балла)
--
eofP :: Parser ()
eofP = Parser $ \case
  [] -> Just ((), "")
  _ -> Nothing

---------------------------------------

-- | 4.c Парсер, который парсит символ lBorder, всё, что парсит переданный парсер p,
--        а потом — правый символ rBorder. Возвращает то, что напарсил парсер p (0,5 балла)
--
inBetweenP :: String -> String -> Parser a -> Parser a
inBetweenP lBorder rBorder p = stringP lBorder *> p <* stringP rBorder

---------------------------------------

-- 4.d Реализуйте функцию listP, которая парсит списки вида "[1, a   , bbb , 23     , -7]" (1 балл)

-- | Вспомогательная функция;
--   принимает на вход 2 парсера: первый парсит элементы, в второй -- разделители
-- 
sepByP :: Parser a -> Parser b -> Parser [a]
sepByP p sep = (:) <$> p <*> some (spaceP *> sep *> spaceP *> p) <|> pure []

-- | Функция принимает парсер, которым парсятся элементы списка
--
listP :: Parser a -> Parser [a]
listP p = inBetweenP "[" "]" (sepByP p (satisfyP (== ',')))

-------------------------------------------------------------------------------

-- 6. Парсим CSV формат (0,75 балл)

-- Самый популярный формат для хранения табличных данных -- CSV (https://en.wikipedia.org/wiki/Comma-separated_values).

-- | CSV представляет собой список названий колонок и список строк со значениями.
--
data CSV = CSV {
    colNames :: [String] -- названия колонок в файле
  , rows     :: [Row]    -- список строк со значениями из файла
  }

-- | Строка CSV представляет собой отображение названий колонок в их значения в данной строке, разделенные запятыми
--   Eсли значения между запятыми нет, распарсить ставим Nothing
--     x,y   --> [Just x, Just y]
--     x,,y  --> [Just x, Nothing, Just y]
--
newtype Row = Row (Map String (Maybe Value))
  deriving (Show)

---------------------------------------

-- | 6.a Реализуйте парсер, который парсит 'Row'. 
--   Названия колонок файла передаются аргументом (0,5 балла)
--
rowP :: [String] -> Parser Row
rowP colNames = undefined

---------------------------------------

-- | 6.b Парсер СSV (0,25 балла)
--       На практике 7 лекции мы разобрали, как писать парсер CSV
--       Скорпируйте его и запустите на вашем rowP -- убедитесь, что все работает
--
csvP :: Parser CSV
csvP = undefined

testIO :: IO (Maybe (CSV, String))
testIO = do
    content <- readFile "files_for_parsing/test.csv"
    return $ runParser csvP content

-------------------------------------------------------------------------------
