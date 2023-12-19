import Control.Applicative (Alternative (..))
import Control.Applicative qualified as Map
import Data.Char (digitToInt, isAlphaNum)
import Data.Foldable (foldl')
import Data.Map.Strict (Map, fromList)
import Data.Maybe (isNothing)
import Data.Text qualified as T
import Distribution.Compat.CharParsing (CharParsing (satisfy), digit)
import Distribution.Simple.Utils (warn, xargs)
import Parser
  ( Parser (..),
    digitP,
    digitsP,
    satisfyP,
    spaceP,
    symbolsP,
  )

-------------------------------------------------------------------------------

-- В этой домашке вам потребуется подгружать одновременно 2 файла в ghci:
-- src/Parser.hs и src/MyLib.hs. Это требует 2 шага:
--
-- ghci> :l src/Parser.hs src/MyLib.hs
-- ghci> :m Parser MyLib

-------------------------------------------------------------------------------

-- 1. Простые парсеры (1,75 балл)

---------------------------------------

testString :: String
testString = "123\n123"

-- | 1.a Парсит новую строку (0,25 балла)
newLineP :: Parser Char
newLineP = satisfyP (== '\n')

---------------------------------------

-- | 1.b Парсит целое число (0,25 балла)
--       (какой парсер из модуля Parser можно переиспользовать?)
-- :P, разобрался
intP :: Parser Int
intP = foldl' (\acc x -> acc * 10 + x) 0 <$> digitsP

intP2 :: Parser Int -- парсит числа формата [1...9][0...9]+
intP2 = foldl' (\acc x -> acc * 10 + x) 0 <$> ((:) <$> firstDigitP <*> digitsP)
  where
    firstDigitP :: Parser Int
    firstDigitP = digitToInt <$> satisfyP (`elem` ['1' .. '9'])

---------------------------------------

-- | 1.c Парсит вещественное число формата `-?(0|[1-9]\d*).\d*` (0,75 балла)
-- :p, разобрался
floatP :: Parser Float
floatP =
  (+)
    <$> (fromIntegral <$> f)
    <* satisfyP (== '.')
    <*> helper
  where
    f = digitToInt <$> satisfyP (== '0') <|> intP
    helper = foldl' (\acc x -> 0.1 * (acc + fromIntegral x)) 0.0 . reverse <$> digitsP

---------------------------------------
-- <* takes left part

-- * > takes right part

-- | 1.d Парсит заданную строку (0,25 балла)
-- :P, разобрался
stringP :: String -> Parser String
stringP str = Parser (helper str)
  where
    helper :: String -> String -> Maybe (String, String)
    helper [] x = Just (str, x)
    helper _ [] = Nothing
    helper (s : ss) (x : xs) = if s == x then helper ss xs else Nothing

-- stringP :: String -> Parser String
-- stringP = foldr (\ x -> (<*>) ((:) <$> satisfyP (== x))) (pure [])

-- stringP :: String -> Parser String
-- stringP []       = pure []
-- stringP (x : xs) = (:) <$> satisfyP (== x) <*> stringP xs

-- myFoldr :: [Int] -> [Int]
-- myFoldr = foldr (\x acc -> x : acc) []

-- myFoldl :: [Int] -> [Int]
-- myFoldl = foldl (\acc x -> x : acc) []

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
intOrFloatP :: Parser (Either [Int] String)
intOrFloatP = Left <$> digitsP <|> Right <$> symbolsP

valueP :: Parser Value
valueP = FloatValue <$> floatP <|> IntValue <$> intP <|> StringValue <$> symbolsP

-------------------------------------------------------------------------------

-- 2. Парсер-комбинаторы и арифметика (1 балла)

-- | Парсеры можно комбинировать
multDigitsP :: Parser Int
multDigitsP =
  (*) -- перемножаем то, что распарсили
    <$> digitP -- парсим цифру
    <* spaceP -- парсим произвольное число пробелов (и не сохраняем их! используя <*)
    <* satisfyP (== '*') -- парсим символ '*' (и не сохраняем его)
    <* spaceP -- парсим произвольное число пробелов (и не сохраняем их)
    <*> digitP -- парсим цифру

runMultDigitsP :: Maybe (Int, String)
runMultDigitsP = runParser multDigitsP "3 * 6" -- Just (18, "")

---------------------------------------

-- 2.a Парсит перемножение чисел (0,5 балла)

-- | Парсер multDigitsP умеет работать только с цифрами, но не с числами
runMultIntsP' :: Maybe (Int, String)
runMultIntsP' = runParser multDigitsP "33 * 6" -- Nothing

-- | Реализуйте парсер, который умеет парсить 2 целых числа и перемножать их
multIntsP :: Parser Int
multIntsP = (*) <$> intP <* spaceP <* satisfyP (== '*') <* spaceP <*> intP

-- | Парсит 2 вещественных числа и перемножает их
multFloatsP :: Parser Float
multFloatsP = (*) <$> floatP <* spaceP <* satisfyP (== '*') <* spaceP <*> floatP

sumFloatsP :: Parser Float
sumFloatsP = (+) <$> floatP <* spaceP <* satisfyP (== '+') <* spaceP <*> floatP

---------------------------------------

-- 2.b Парсит перемножение/сложение 2 вещественных чисел (0,5 балла)

-- | Зададим простейшее выражение:
--   левый и правый операнд -- Float, операция -- Сhar
data SimpleExpr = SimpleExpr Float Char Float
  deriving (Show, Eq)

-- | Реализуйте парсер для SimpleExpr; операция может быть '+' и '*'
simpleExprP :: Parser SimpleExpr
simpleExprP =
  SimpleExpr
    <$> floatP
    <* spaceP
    <*> satisfyP (\a -> a == '+' || a == '*')
    <* spaceP
    <*> floatP

-- simpleExprP :: Parser Float
-- simpleExprP = multFloatsP <|> sumFloatsP

-- | Более сложное выражение, в котором операция задана типом данных Operation
data Expr = Expr
  { left :: Float,
    op :: Operation,
    right :: Float
  }
  deriving (Show, Eq)

data Operation = Mult | Sum
  deriving (Show, Eq)

-- | Реализуйте парсер для Expr
-- exprP :: Parser Expr
-- exprP = Expr
--   <$> floatP
--   <* spaceP
--   <*> ((Sum <$ satisfyP (== '+')) <|> (Mult <$ satisfyP (== '*')))
--   <* spaceP
--   <*> floatP

-- | Реализуйте парсер, который парсит перемножение/сложение 2 вещественных чисел.
--   Используйте exprP
exprP :: Parser Expr
exprP = Expr <$> floatP <* spaceP <*> ((Sum <$ satisfyP (== '+')) <|> (Mult <$ satisfyP (== '*'))) <* spaceP <*> floatP

sumMultFloatsP :: Parser Float
sumMultFloatsP = f <$> exprP
  where
    f :: Expr -> Float
    f expr = case op expr of
      Sum -> left expr + right expr
      Mult -> left expr * right expr

-------------------------------------------------------------------------------

-- 3. fmap<n>, <$><*> и парсер-комбинаторы (0,75 балла)

---------------------------------------

-- 3.a | Реализуйте fmap4 через fmap -- прокомментируйте каждый шаг указанием типов (0,5 балла)
-- :p, разобрался

fmap4 :: (Applicative f) => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
fmap4 f fa fb fc fd = f <$> fa <*> fb <*> fc <*> fd

-- <$> :: (x -> y) -> f x -> f y
-- <*> :: f (x -> y) -> f x -> f y

-- f :: (a -> b - > c -> d -> e) = (a -> (b -> c -> d -> e))
-- x = a, y = (b -> c -> d -> e)
-- <$> :: (x -> y) -> f x -> f y
-- fmap :: (a -> (b -> c -> d -> e)) -> f a -> f (b -> c -> d -> e)
-- fmap f :: f a -> f (b -> c -> d -> e)
-- ffa = f <$> fa :: f (b -> c -> d -> e)

-- ffa :: f (b -> c -> d -> e) = f (b -> (c -> d -> e))
-- x = b, y = (c -> d -> e)
-- ffa <*> :: f b -> f (c -> d -> e)
-- ffaFb = ffa <*> fb :: f (c -> d -> e)

-- ffafb :: f (c -> d -> e) = f (c -> (d -> e))
-- x = c, y = (d -> e)
-- ffaFb <*> :: f c -> f (d -> e)
-- ffaFbFc = ffaFb <*> fc :: f (d -> e)

-- ffaFbFc :: f (d -> e)
-- x = d, y = e
-- ffaFbFc <*> :: f d -> f e
-- ffaFbFcFd = ffaFbFc <*> fd :: f e

---------------------------------------
-- indeed they both have the same precedence
-- (infixl 4: (<*>) and (<$>)) and you can just read it from left to right -

-- 3.b Сравните реализацию fmap4 и multDigitsP/simpleExprP/exprP -- видите похожий паттерн?
--     Поделитесь своими мыслями / наблюдениями на этот счет (0,25 балла)

-- f <$> fa <*> fb <*> fc <*> fd
-- (+) <$> floatP <* spaceP <* satisfyP (== '+') <* spaceP <*> floatP

-- при работе с парсерами:
-- видим, что мы сначала применяем функцию без контекста к нашему парсер с помощью <$>
-- далее уже оба значения в контексте, поэтому работаем с помощью <*>

-- по сути то же самое происходит и в примере с fmap4
-------------------------------------------------------------------------------

-- 4. Более сложные парсеры (2,25 балла)

---------------------------------------

-- | 4.a Парсит весь поток, пока символы потока удовлетворяют заданному условию (0,25 балла)
takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP p = some (satisfyP p)

---------------------------------------

-- | 4.b Парсер, который фейлится, если в потоке что-то осталось.
--       В противном случае парсер отрабатывает успешно (0,5 балла)

-- :p, как понять, что тут происходит?
eofP :: Parser ()
eofP = Parser f
  where
    f :: String -> Maybe ((), String)
    f [] = Just ((), [])
    f _ = Nothing

-- symbolP :: Parser Char
-- symbolP = satisfyP isAlphaNum

---------------------------------------

-- | 4.c Парсер, который парсит символ lBorder, всё, что парсит переданный парсер p,
--        а потом — правый символ rBorder. Возвращает то, что напарсил парсер p (0,5 балла)

-- inBetweenP :: Char -> Char -> Parser a -> Parser a
-- inBetweenP lBorder rBorder p = takeWhileP (== lBorder) *> p <* takeWhileP (== rBorder)

inBetweenP :: String -> String -> Parser a -> Parser a
inBetweenP lBorder rBorder p = stringP lBorder *> p <* stringP rBorder

---------------------------------------

-- 4.d Реализуйте функцию listP, которая парсит списки вида "[1, a   , bbb , 23     , -7]" (1 балл)

-- | Вспомогательная функция;
--   принимает на вход 2 парсера: первый парсит элементы, в второй -- разделители

-- sepByP :: Parser a -> Parser b -> Parser [a]
-- sepByP p sep = satisfyP (== '[') *> (some (p <* sep) <|> some p) <* satisfyP (== ']')

sepByP :: Parser a -> Parser b -> Parser [a]
sepByP p sep = (:) <$> p <*> some (spaceP <* sep <* spaceP *> p)

-- | Функция принимает парсер, которым парсятся элементы списка
listP :: Parser a -> Parser [a]
listP p = inBetweenP "[" "]" $ sepByP p (satisfyP (== ',')) <|> pure []

-------------------------------------------------------------------------------

-- 6. Парсим CSV формат (0,75 балл)

-- Самый популярный формат для хранения табличных данных -- CSV (https://en.wikipedia.org/wiki/Comma-separated_values).

-- | CSV представляет собой список названий колонок и список строк со значениями.
data CSV = CSV
  { colNames :: [String], -- названия колонок в файле
    rows :: [Row] -- список строк со значениями из файла
  }

instance Show CSV where
  show :: Str

-- | Строка CSV представляет собой отображение названий колонок в их значения в данной строке, разделенные запятыми
--   Eсли значения между запятыми нет, распарсить ставим Nothing
--     x,y   --> [Just x, Just y]
--     x,,y  --> [Just x, Nothing, Just y]
newtype Row = Row (Map String (Maybe Value))
  deriving (Show)

---------------------------------------

-- | 6.a Реализуйте парсер, который парсит 'Row'.
--   Названия колонок файла передаются аргументом (0,5 балла)

-- ٩(ˊ〇ˋ*)و
rowP :: [String] -> Parser Row
rowP colnames = Row . fromList . zip colnames <$> rowMaybeP
  where
    value = (Just <$> valueP) <|> pure Nothing
    rowMaybeP = (:) <$> value <*> many helpP
      where
        helpP = spaceP <* satisfyP (== ',') <* spaceP *> value

---------------------------------------

-- | 6.b Парсер СSV (0,25 балла)
--       На практике 7 лекции мы разобрали, как писать парсер CSV
--       Скорпируйте его и запустите на вашем rowP -- убедитесь, что все работает

-- (っ˘ڡ˘ς)
csvP :: Parser CSV
csvP = Parser f
  where
    f :: String -> Maybe (CSV, String)
    f s = case runParser colNamesP s of
      Nothing -> Nothing
      Just (cols, s') -> case runParser (rowsP cols <|> pure []) s' of
        Nothing -> Nothing
        Just (rowss, s'') -> Just (CSV cols rowss, s'')

    colNamesP :: Parser [String]
    colNamesP = sepByP symbolsP (satisfyP (== ','))

    rowsP :: [String] -> Parser [Row]
    rowsP cNames = many (satisfyP (== '\n') *> rowP cNames)

testIO :: IO (Maybe (CSV, String))
testIO = do
  content <- readFile "files_for_parsing/test.csv"
  return $ runParser csvP content

-------------------------------------------------------------------------------
