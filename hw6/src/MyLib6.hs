module MyLib6 where
import           Control.Applicative (Alternative (..))
import           Data.Foldable       (foldl')
import           Data.Map.Strict     (Map, fromList)
import qualified Data.Text           as T
import           Parser
import Control.Applicative
import Data.Char (digitToInt)
import Data.List (stripPrefix)



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
intP = foldl' (\acc x -> acc * 10 + x) 0 <$> digitsP

---------------------------------------

-- | 1.c Парсит вещественное число формата `-?(0|[1-9]\d*).\d*` (0,75 балла)
--
floatP :: Parser Float
floatP = (+) <$> integralPart <*> decimalPart
  where
    integralPart = fromIntegral <$> readFirst <* satisfyP (== '.')
    decimalPart = (\x -> fromIntegral x / 10 ^ length (show x)) <$> intPart <|> pure 0
    intPart = intP
    readFirst = digitToInt <$> satisfyP (== '0') <|> intP

---------------------------------------

-- | 1.d Парсит заданную строку (0,25 балла)
--
stringP :: String -> Parser String
stringP str = Parser go
  where
    go :: String -> Maybe (String, String)
    go input = case stripPrefix str input of
      Just rest -> Just (str, rest)
      Nothing   -> Nothing
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
digitsToInt :: [Int] -> Int
digitsToInt = foldl (\acc x -> acc * 10 + x) 0

intValueP :: Parser Value
intValueP = IntValue . digitsToInt <$> digitsP



symbolToString :: Parser String
symbolToString = some symbolP

stringValueP :: Parser Value
stringValueP = StringValue <$> symbolToString

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
valueP :: Parser Value
valueP = FloatValue <$> floatP <|> IntValue <$> intP <|> StringValue <$> symbolsP

-------------------------------------------------------------------------------

-- -- 2. Парсер-комбинаторы и арифметика (1 балла)

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

-- -- 2.a Парсит перемножение чисел (0,5 балла)

-- -- | Парсер multDigitsP умеет работать только с цифрами, но не с числами
-- --
runMultIntsP' :: Maybe (Int, String)
runMultIntsP' = runParser multDigitsP "33 * 6" -- Nothing

-- -- | Реализуйте парсер, который умеет парсить 2 целых числа и перемножать их
-- -- 
multIntsP :: Parser Int
multIntsP = (*) <$> (intP <* spaceP <* satisfyP (== '*') <* spaceP) <*> intP

-- | Парсит 2 вещественных числа и перемножает их
-- 
multFloatsP :: Parser Float
multFloatsP = (*) <$> (floatP <* spaceP <* satisfyP (== '*') <* spaceP) <*> floatP

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
  <* spaceP
  <*> satisfyP (\a -> a == '+' || a == '*')
  <* spaceP
  <*> floatP

-- -------------------------------------------------------------------------------