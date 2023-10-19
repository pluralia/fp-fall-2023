-- без этого cabal build отказывался собираться
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-} 

-- lhint  дал только одно замечание к 3-му заданию, я его в комментариях прописала

module MyLib where

import Data.Ix
------------------------------------------------------------------------------------------------

-- 1. Числа Черча и `Ix` (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча
data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

instance Eq ChurchNumber where
    Zero     == Zero     = True
    (Succ m) == (Succ n) = m == n
    _        == _        = False

instance Ord ChurchNumber where
  compare Zero     Zero     = EQ
  compare Zero     (Succ _) = LT
  compare (Succ _) Zero     = GT
  compare (Succ x) (Succ y) = compare x y

instance Num ChurchNumber where
    fromInteger n
        | n < 0     = error "negative!"
        | n == 0    = Zero
        | otherwise = Succ (fromInteger (n - 1))

    (+) m Zero     = m
    (+) m (Succ n) = Succ (m + n)

    (-) m Zero            = m
    (-) Zero     _        = Zero
    (-) (Succ m) (Succ n) = m - n

    (*) Zero _        = Zero
    (*) _    Zero     = Zero
    (*) m    (Succ n) = m + (m * n)

    abs             = id
    signum Zero     = Zero
    signum (Succ _) = Succ Zero

-- я не знаю, как реализовать Ix 
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

-- честно пользовалась гуглом, до конца так и не разобралась, как это работает именно в инстансах haskell 
newtype InOrder a = In (Tree a)

instance Show a => Show (InOrder a) where
    show (In tree) = inOrderShow tree
      where
        inOrderShow (Node val [])    = show val
        inOrderShow (Node val sTree) = inOrderSTree sTree ++ show val ++ inOrderSTree (drop 1 sTree)
        inOrderSTree []              = ""
        inOrderSTree (t:ts)          = inOrderShow t ++ inOrderSTree ts

-- | pre-order обход дерева
--
newtype PreOrder a = Pre (Tree a)

instance Show a => Show (PreOrder a) where
    show :: Show a => PreOrder a -> String
    show (Pre tree) = preOrderShow tree
      where
        preOrderShow (Node val [])    = show val
        preOrderShow (Node val sTree) = show val ++ preOrderSTree sTree
        preOrderSTree []              = ""
        preOrderSTree (t:ts)          = preOrderShow t ++ preOrderSTree ts


-- | post-order обход дерева
--
newtype PostOrder a = Post (Tree a)

instance Show a => Show (PostOrder a) where
    show :: Show a => PostOrder a -> String
    show (Post tree) = postOrderShow tree
      where
        postOrderShow (Node val [])    = show val
        postOrderShow (Node val sTree) = postOrderSTree sTree ++ show val
        postOrderSTree []              = ""
        postOrderSTree (t:ts)          = postOrderShow t ++ postOrderSTree ts

---------------------------------------

-- 2.b Напишите инстанс `Eq` для дерева (0,5 балла)

instance (Eq a) => Eq (Tree a) where
  (Node val1 child1) == (Node val2 child2) = val1 == val2 && child1 == child2

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
  | inRange (0, 255) `all` [red, green, blue] = Just $ UnsafeMkRGB (fromIntegral red) (fromIntegral green) (fromIntegral blue)
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

-- я взяла формулы из https://www.101computing.net/cmyk-to-rgb-conversion-algorithm/
-- они дают float при делении, и я поменяла Ваш конструктор mkRGB чтобы можно было работать с float, (добавила fromIntegral) 
-- но в итоге ошибка при попытке выполнить из терминала что-то вроде
-- toCMYK (UnsafeMkRGB 125 130 160) -> *** Exception: divide by zero
-- и общая ошибка при сборке
-- поэтому тестов тоже нет, так как я не разобралась, что не так

-- еще у меня огромное количество ворнингов при сборке и тестировании на это задание с [-Wname-shadowing] и warning: [-Wincomplete-patterns]
-- но мы переменные изначально так задаем для читаемости, поэтому ничего не делала


class ToCMYK a where
    toCMYK :: a -> Maybe CMYK

instance ToCMYK [Int] where
    toCMYK [cyan, magenta, yellow, black]
        | inRange (0, 100) `all` [cyan, magenta, yellow, black] = Just $ UnsafeMkCMYK (fromIntegral cyan) (fromIntegral magenta) (fromIntegral yellow) (fromIntegral black)
        | otherwise                                             = Nothing

instance ToCMYK RGB where
    toCMYK (UnsafeMkRGB r g b)
      | inRange (0, 255) `all` [r, g, b] = mkCMYK c m y k
      | otherwise = Nothing
      where
        k = 1 - maximum [r, g, b] `div` 255
        c = (1 - r `div` 255) `div` (1 - k)
        m = (1 - g `div` 255) `div` (1 - k)
        y = (1 - b `div` 255) `div` (1 - k)


---------------------------------------

-- 3.b Классы типов -- это функции (1 балл)

-- | задайте класс типов ToCMYK используя data (аналогично Eq' из практики)
--
newtype DToCMYK a = MkDToCMYK { toCMYK' :: a -> Maybe CMYK }

{-
здесь hlint предлагал заменить
  data DToCMYK a = MkDToCMYK {toCMYK' :: a -> Maybe CMYK}
Perhaps:
  newtype DToCMYK a = MkDToCMYK {toCMYK' :: a -> Maybe CMYK}
Note: decreases laziness
я почитала и, если правильно поняла, newtype больше подходит для изоморфных структур, когда у нас много вариантов типов
здесь только один, но также еще можно заменой уменьшить ленивость, поэтому заменила
-}

-- | реализуйте класс типов DToCMYK для [Int] и для RGB в виде функций (аналогично dEqBool из практики)

dToCMYKList :: DToCMYK [Int]
dToCMYKList = MkDToCMYK toCMYKList
  where
    toCMYKList [r, g, b]
        | inRange (0, 255) r && inRange (0, 255) g && inRange (0, 255) b = Just $ UnsafeMkCMYK c m y k
        | otherwise = Nothing
        where
            k = 1 - maximum [r, g, b] `div` 255
            c = (1 - r `div` 255) `div` (1 - k)
            m = (1 - g `div` 255) `div` (1 - k)
            y = (1 - b `div` 255) `div` (1 - k)

dToCMYKRGB :: DToCMYK RGB
dToCMYKRGB = MkDToCMYK toCMYKRGB
  where
    toCMYKRGB (UnsafeMkRGB r g b)
        | inRange (0, 255) `all` [r, g, b] = Just $ UnsafeMkCMYK c m y k
        | otherwise = Nothing
        where
            k = 1 - maximum [r, g, b] `div` 255
            c = (1 - r `div` 255) `div` (1 - k)
            m = (1 - g `div` 255) `div` (1 - k)
            y = (1 - b `div` 255) `div` (1 - k)

---------------------------------------

-- 3.c Используйте инстансы (0,25 балла)
--     Приведите пример использования инстансов [Int] и RGB реализованных в 3a и 3b (должно получится 4 примера)
-- с учетом комментария из 3.а - здесь у меня ничего не работает

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

pointful a b c d = a b (c d)
{-
a b (c d) -> a b $ c d - замена скобок на оператор апликации $ 
a b $ c d -> a b . c $ d - замена оператора апликации $ на оператор композиции, перенос оператора апликации $ на переменную. Можно дропать
a b . c $ d -> a b . c
* в практике Вы написали, что f . g x = (.) f (g x) = ((.) f) (g x), тогда
a b . c -> (.) (a b) (.) c - это по строчке выше
(.) (a b) . c -> (.) $ a b .  - замена скобок на оператор апликации $ и дроп с
(.) $ a b . c -> (.) a . - замена оператора апликации $ на оператор композиции, перенос оператора апликации $ на переменную и дроп
(.) a . -> (.) (.) . - перенос по * и дроп а
-}

------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq)

instance Enum Day where
  toEnum n = case n `mod` 7 of
    0 -> Sun
    1 -> Mon
    2 -> Tue
    3 -> Wed
    4 -> Thu
    5 -> Fri
    6 -> Sat
    _ -> error "Invalid day value"
  
  fromEnum name = case name of
    Mon -> 1
    Tue -> 2
    Wed -> 3
    Thu -> 4
    Fri -> 5
    Sat -> 6
    Sun -> 0

---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
--
nextDay :: Day -> Day
nextDay day = toEnum $ (fromEnum day + 1) `mod` 7

-- | Возвращает предыдущий день
--
dayBefore :: Day -> Day
dayBefore day = toEnum $ (fromEnum day - 1) `mod` 7

-- | Возвращает количество от текущего до ближайшей субботы
--
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend day = (fromEnum Sat - fromEnum day) `mod` 7

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
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева из задания 2 (0,5 балла)

instance Functor Tree where
  fmap f (Node val child) = Node (f val) (map (fmap f) child)

---------------------------------------

-- 6.c(*) Реализуйте инстанс Functor для пары (1 балл)

data Pair a b = Pair a b
  deriving (Show)

-- С какими трудностями вы столкнулись?

------------------------------------------------------------------------------------------------

-- 7. Класс типов `Bifunctor` (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Functor для Either и пары

------------------------------------------------------------------------------------------------

-- 8. Задайте свой класс типов, опираясь на интересы, хобби и фазу Луны (0,25 балла)


------------------------------------------------------------------------------------------------
