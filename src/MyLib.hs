module MyLib where

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

import Data.Ix
import Data.Bifunctor

------------------------------------------------------------------------------------------------

-- 1. Числа Черча и `Ix` (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча
data ChurchNumber = Zero | Succ ChurchNumber
--  deriving (Show, Eq)

instance Eq ChurchNumber where
  Zero == Zero = True
  Zero == _    = False
  _ == Zero    = False 
  (Succ m) == (Succ n) = n == m

-- Функция преобразования числа Чёрча в число обычное
chToInt :: ChurchNumber -> Integer
chToInt Zero     = 0
chToInt (Succ n) = 1 + chToInt n

instance Ord ChurchNumber where
  compare n1 n2 = compare (chToInt n1) (chToInt n2)


-- Сначала все функции из hw1 для вычисления чисел Чёрча

-- | The successor of a Church numeral
chSucc :: ChurchNumber -> ChurchNumber
chSucc = Succ

-- | Addition of two Church numerals
chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd (Succ m) n = chAdd m (chSucc n)
chAdd Zero n = n

-- | Multiplication of two Church numerals
chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult (Succ m) n = chAdd n (chMult m n)
chMult Zero n = Zero

-- | Raising to the power of a Church numeral
chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow Zero n = chSucc Zero
chPow (Succ m) n = chMult n (chPow m n)


-- | Previous of a Church numeral
chPrev :: ChurchNumber -> ChurchNumber
chPrev (Succ (Succ n)) = Succ n
chPrev _ = Zero

-- Функция для вычитания одного числа Чёрча из другого
-- я прочитал, что в хаскеле можно с помощью @ как бы связывать имена и написал так функцию
-- но можно и без строчки chSubtract a@(Succ m) b@(Succ n), тогда:
-- chSubtract (Succ m) (Succ n)
-- | (Succ m) == (Succ n) = Zero
-- | otherwise = chSubtract m (Succ n)
chSubtract :: ChurchNumber -> ChurchNumber -> ChurchNumber
chSubtract a@(Succ m) b@(Succ n)
  | a == b = Zero
  | otherwise = chSubtract m b


-- Функция для создания числа Чёрча из Integer
chFromInteger :: Integer -> ChurchNumber
chFromInteger n
  | n == 0    = Zero
  | n > 0     = Succ (chFromInteger (n - 1))
  | otherwise = error "Na-ah, u can`t have negative rusult, buddy"

-- Теперь, собственно, сам инстанс (брал функции из hoogl`а)

instance Num ChurchNumber where
    -- Сложение двух чисел Чёрча
    (+) = chAdd

    -- Умножение двух чисел Чёрча
    (*) = chMult

    -- Модуль числа Чёрча
    abs = id

    -- Знак числа Чёрча (всегда положителен)
    signum _ = Succ Zero

    -- Операция перевода числа в число Чёрча
    fromInteger n
     | n < 0     = error "Na-ah, u can`t have negative rusult, buddy"
     | otherwise = chFromInteger n

    -- Негирование числа Чёрча (они все положительные, так что это всегда ноль)
    negate _ = Zero

    -- Вычитание одного числа Чёрча из другого
    (-) a b
      | a >= b    = chSubtract a b
      | otherwise = error "Na-ah, u can`t have negative rusult, buddy"

-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

instance Enum ChurchNumber where
    toEnum 0 = Zero
    toEnum n
        | n > 0 = Succ (toEnum (n - 1))
        | otherwise = error "toEnum: negative argument"
    
    fromEnum Zero = 0
    fromEnum (Succ n) = 1 + fromEnum n

instance Ix ChurchNumber where
    range (m, n) = map chFromInteger [intM..intN]
        where
            intM = chToInt m
            intN = chToInt n

    index (m, n) i
        | i < m || i > n = error "Woopsi, wrong number"
        | i == m = 0
        | otherwise = 1 + index (m, n) (pred i)

    inRange (m, n) i = i >= m && i <= n

------------------------------------------------------------------------------------------------

-- 2. Дерево (2 балла)

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  }

---------------------------------------

-- 2.a Напишите 3 инстанса `Show` для дерева.
--     Истансы должны реализовывать in-, pre-, post-order обход дерева (1,5 балла)

-- Мы не можем определять несколько инстансов одного типа для одного класса,
-- поэтому обернем наш тип `Tree` в newtype, чтобы у нас было 3 разных типа,
-- и уже для них реализуем `Show`

-- | in-order обход дерева
--
newtype InOrder a = In (Tree a)

instance Show a => Show (InOrder a) where
    show (In tree) = inOrderShow tree

-- Вспомогательная функция для in-order обхода дерева
inOrderShow :: Show a => Tree a -> String
inOrderShow (Node val subtrees) = inOrderSubtrees subtrees ++ show val

-- Вспомогательная функция для обхода поддеревьев в in-order
inOrderSubtrees :: Show a => [Tree a] -> String
inOrderSubtrees [] = ""
inOrderSubtrees [t] = inOrderShow t
inOrderSubtrees (t:ts) = inOrderShow t ++ inOrderSubtrees ts

-- | pre-order обход дерева
--
newtype PreOrder a = Pre (Tree a)

instance Show a => Show (PreOrder a) where
    show (Pre tree) = preOrderShow tree

-- Вспомогательная функция для pre-order обхода дерева
preOrderShow :: Show a => Tree a -> String
preOrderShow (Node val subtrees) = show val ++ preOrderSubtrees subtrees

-- Вспомогательная функция для обхода поддеревьев в pre-order
preOrderSubtrees :: Show a => [Tree a] -> String
preOrderSubtrees [] = ""
preOrderSubtrees (t:ts) = preOrderShow t ++ preOrderSubtrees ts


-- | post-order обход дерева
--
newtype PostOrder a = Post (Tree a)

instance Show a => Show (PostOrder a) where
    show (Post tree) = postOrderTraversal tree
        where
            postOrderTraversal :: Show a => Tree a -> String
            postOrderTraversal (Node val children) = concatMap postOrderTraversal children ++ show val ++ " "


---------------------------------------

-- 2.b Напишите инстанс `Eq` для дерева (0,5 балла)

instance Eq a => Eq (Tree a) where
    (Node val1 children1) == (Node val2 children2) = val1 == val2 && children1 == children2

------------------------------------------------------------------------------------------------

-- 3. Цвета (2 балла)

-- | Зададим тип данных RGB, но так как его поля должны быть от 0 до 255, назовем конструктор Unsafe...
--
data RGB = UnsafeMkRGB
  { red   :: Int
  , green :: Int
  , blue  :: Int
  } deriving (Show)

-- | ...и зададим новый конструктор, который будет проверять значения полей при инициализации
--
mkRGB :: Int -> Int -> Int -> Maybe RGB
mkRGB red green blue
  | inRange (0, 255) `all` [red, green, blue] = Just $ UnsafeMkRGB red green blue
  | otherwise                                 = Nothing 

-- | Аналогично поступим, задавая тип данных CMYK
--
data CMYK = UnsafeMkCMYK
  { cyan    :: Int
  , magenta :: Int
  , yellow  :: Int
  , black   :: Int
  } deriving (Show)

mkCMYK :: Int -> Int -> Int -> Int -> Maybe CMYK
mkCMYK cyan magenta yellow black
  | inRange (0, 100) `all` [cyan, magenta, yellow, black] = Just $ UnsafeMkCMYK cyan magenta yellow black
  | otherwise                                             = Nothing 

---------------------------------------

-- 3.a Напишите инстансы класса ToCMYK для [Int] и для RGB (0,75 балла)

class ToCMYK a where
    toCMYK :: a -> Maybe CMYK

instance ToCMYK [Int] where
  toCMYK :: [Int] -> Maybe CMYK
  toCMYK [cyan, magenta, yellow, black]
    | inRange (0, 100) `all` [cyan, magenta, yellow, black] = Just $ UnsafeMkCMYK cyan magenta yellow black
    | otherwise = Nothing
  toCMYK _ = Nothing
  
instance ToCMYK RGB where
  toCMYK :: RGB -> Maybe CMYK
  toCMYK (UnsafeMkRGB red green blue) 
    | inRange (0, 100) `all` [red, green, blue] = Just $ UnsafeMkCMYK cyan magenta yellow black
    | otherwise = Nothing
    where
      r = fromIntegral red / 255.0
      g = fromIntegral green / 255.0
      b = fromIntegral blue / 255.0
      k = 1 - maximum [r, g, b]
      c = (1 - r - k) / (1 - k)
      m = (1 - g - k) / (1 - k)
      y = (1 - b - k) / (1 - k)
      cyan = round (c * 100)
      magenta = round (m * 100)
      yellow = round (y * 100)
      black = round (k * 100)

---------------------------------------

-- 3.b Классы типов -- это функции (1 балл)

-- | задайте класс типов ToCMYK используя data (аналогично Eq' из практики)
--
data DToCMYK a = MkDToCMYK { toCMYK' :: a -> Maybe CMYK }

-- | реализуйте класс типов DToCMYK для [Int] и для RGB в виде функций (аналогично dEqBool из практики)

dToCMYKList :: DToCMYK [Int]
dToCMYKList = MkDToCMYK toCMYKList
  where
    toCMYKList [red, green, blue]
        | checkRange [red, green, blue] = Just $ UnsafeMkCMYK cyan magenta yellow black
        | otherwise = Nothing
        where
          r = fromIntegral red / 255.0
          g = fromIntegral green / 255.0
          b = fromIntegral blue / 255.0
          k = 1 - maximum [r, g, b]
          c = (1 - r - k) / (1 - k)
          m = (1 - g - k) / (1 - k)
          y = (1 - b - k) / (1 - k)
          cyan = round (c * 100)
          magenta = round (m * 100)
          yellow = round (y * 100)
          black = round (k * 100)


checkRange :: [Int] -> Bool
checkRange = all (\x -> x >= 0 && x <= 255)


dToCMYKRGB :: DToCMYK RGB
dToCMYKRGB = MkDToCMYK toCMYKRGB
  where
    toCMYKRGB (UnsafeMkRGB red green blue)
        | checkRange [red, green, blue] = Just $ UnsafeMkCMYK cyan magenta yellow black
        | otherwise = Nothing
        where
          r = fromIntegral red / 255.0
          g = fromIntegral green / 255.0
          b = fromIntegral blue / 255.0
          k = 1 - maximum [r, g, b]
          c = (1 - r - k) / (1 - k)
          m = (1 - g - k) / (1 - k)
          y = (1 - b - k) / (1 - k)
          cyan = round (c * 100)
          magenta = round (m * 100)
          yellow = round (y * 100)
          black = round (k * 100)

---------------------------------------

-- 3.c Используйте инстансы (0,25 балла)
--     Приведите пример использования инстансов [Int] и RGB реализованных в 3a и 3b (должно получится 4 примера)

-- Лежат в TestSpec.hs


------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

-- pointful a b c d = a b (c d)
-- pointful a b c d = a b $ c d       -- заменяем скобки на $
-- pointful a b c   = a b $ c         -- удаляем d
-- pointful a b c   = (a b) . c       -- используем оператор композиции
-- pointful a b c   = (.) (a b) c     -- используем его как функцию, которая принимает два аргумента (a b) и с
-- pointful a b     = (.) (a b)       -- удаляем с
-- pointful a b     = (.) a $ b       -- заменяем скобки на $
-- pointful a b     = ((.) a) . b     -- оператор композиции
-- pointful a b     = (.) ((.) a) b   -- используем его как функцию 
-- pointful a       = (.) ((.) a)     -- удаляем b
-- pointful a       = (.) (.) $ a     -- заменяем скобки на $
-- pointful a       = ((.) (.)) . a   -- используем оператор композиции
-- pointful a       = (.) ((.) (.)) a -- используем его как функцию, которая принимает два аргумента ((.) (.)) и а
-- pointful         = (.) ((.) (.))   -- удаляем а


-- Финал: pointful = (.) ((.) (.))

------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq)

-- Indian codewriter style
instance Enum Day where
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  toEnum _ = error "Whats wrong with u? U have more than 7 days in a week?"


  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
--
nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay day    = toEnum $ (fromEnum day + 1)

-- | Возвращает предыдущий день
--
dayBefore :: Day -> Day
dayBefore Monday = Sunday
dayBefore day    = toEnum $ (fromEnum day - 1)

-- | Возвращает количество от текущего до ближайшей субботы
--
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend day = (fromEnum Saturday - fromEnum day) `mod` 7

------------------------------------------------------------------------------------------------

-- 6. Класс типов `Functor` (1,75 балла)

-- Вы уже знакомы с функцией `map` -- мы писали ее для различных типов данных (mapMaybe, mapList)
-- На самом деле она задана в классе типов `Functor`
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Functor.html

---------------------------------------

-- 6.a Реализуйте инстанс Functor для списка (0,25 балла)

data List a = Nil | Cons a (List a)
  deriving (Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil  
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева из задания 2 (0,5 балла)

-- data Tree a = Node
--   { value :: a
--   , children :: [Tree a]
--   }

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node val children) = Node (f val) (map (fmap f) children)

---------------------------------------

-- 6.c(*) Реализуйте инстанс Functor для пары (1 балл)

data Pair a b = Pair a b
  deriving (Show)

instance Functor (Pair a) where
    fmap :: (b -> c) -> Pair a b -> Pair a c
    fmap f (Pair a b) = Pair a (f b)

-- С какими трудностями вы столкнулись?

-- Pair неизменяемый тип данных, так что нельзя просто применить что-то к элементам пары
-- Почитал stakoverflow, там по первой ссылке в гугле "functor for pair haskell" хорошо объяснено
-- почему и как эта штука работает и есть прикольный пример на это main = print $ fmap (*2) ("funny",2)

------------------------------------------------------------------------------------------------

-- 7. Класс типов `Bifunctor` (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

data Either' a b = Left' a | Right' b
  deriving (Show)

instance Bifunctor Either' where
  bimap :: (a -> c) -> (b -> d) -> Either' a b -> Either' c d
  bimap f _ (Left' a) = Left' (f a) 
  bimap _ g (Right' b) = Right' (g b)

instance Bifunctor Pair where
  bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
  bimap f g (Pair a b) = Pair (f a) (g b)

------------------------------------------------------------------------------------------------

-- 8. Задайте свой класс типов, опираясь на интересы, хобби и фазу Луны (0,25 балла)

class VibeCheck a where
    checker :: a -> String

instance VibeCheck String where
    checker "Introvert"  = "Nice, u already perfect"
    checker "Extravert"  = "Well, I guess someone should be this talkative"
    checker "Ambivert"   = "OKAY"
    checker _            = "^__________________________^"

------------------------------------------------------------------------------------------------
