{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib where

import Data.Ix
import Data.Bifunctor
------------------------------------------------------------------------------------------------

-- 1. Числа Черча и `Ix` (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча
data ChurchNumber = Zero | Succ ChurchNumber
--  deriving (Show, Eq)

instance Eq ChurchNumber where
    (==) :: ChurchNumber -> ChurchNumber -> Bool
    Zero     == Zero     = True
    (Succ m) == (Succ n) = m == n
    _        == _        = False

instance Ord ChurchNumber where
  compare :: ChurchNumber -> ChurchNumber -> Ordering
  compare Zero     Zero     = EQ
  compare Zero     (Succ _) = LT
  compare (Succ _) Zero     = GT
  compare (Succ x) (Succ y) = compare x y

instance Num ChurchNumber where
    fromInteger :: Integer -> ChurchNumber
    (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
    (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
    (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
    abs :: ChurchNumber -> ChurchNumber
    signum :: ChurchNumber -> ChurchNumber
    fromInteger n
        | n < 0     = error "Church numbers must be non-negative"
        | n == 0    = Zero
        | otherwise = Succ (fromInteger (n - 1))

    (+) m Zero     = m
    (+) m (Succ n) = Succ (m + n)

    (-) m        Zero     = m
    (-) Zero     _        = Zero
    (-) (Succ m) (Succ n) = m - n

    (*) Zero _        = Zero
    (*) _    Zero     = Zero
    (*) m    (Succ n) = m + (m * n)

    abs             = id  -- abs is the identity function because Church numbers are non-negative.
    signum Zero     = Zero
    signum (Succ _) = Succ Zero

instance Show ChurchNumber where
  show Zero     = "Zero"
  show (Succ n) = "Succ (" ++ show n ++ ")"

instance Enum ChurchNumber where
    toEnum 0 = Zero
    toEnum n
        | n > 0 = Succ (toEnum (n - 1))
        | otherwise = error "toEnum: negative argument"
    
    fromEnum Zero = 0
    fromEnum (Succ n) = 1 + fromEnum n

instance Ix ChurchNumber where
    range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
    index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
    inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool
    range (m, n) = [m..n]
    
    index (m, n) i
        | i < m || i > n = error "Index out of range"
        | i == m = 0
        | otherwise = 1 + index (m, n) (pred i)
    
    inRange (m, n) i = i >= m && i <= n

-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

------------------------------------------------------------------------------------------------

-- 2. Дерево (2 балла)

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  } deriving (Show)

---------------------------------------

-- 2.a Напишите 3 инстанса `Show` для дерева.
--     Истансы должны реализовывать in-, pre-, post-order обход дерева (1,5 балла)

-- Мы не можем определять несколько инстансов одного типа для одного класса,
-- поэтому обернем наш тип `Tree` в newtype, чтобы у нас было 3 разных типа,
-- и уже для них реализуем `Show`

newtype InOrder a = In (Tree a)

instance Show a => Show (InOrder a) where
  show (In tree) = inOrderShow tree
    where
      inOrderShow (Node val childNodes) =  -- Переименовали 'children' в 'childNodes'
        concatMap inOrderShow childNodes ++ show val


-- Определяем новый тип PreOrder для pre-order обхода
newtype PreOrder a = Pre (Tree a)

instance Show a => Show (PreOrder a) where
    show (Pre tree) = preOrderShow tree
      where
        preOrderShow (Node val [])       = show val
        preOrderShow (Node val subtrees) = show val ++ preOrderSubtrees subtrees
        preOrderSubtrees []              = ""
        preOrderSubtrees (t:ts)          = preOrderShow t ++ preOrderSubtrees ts

-- Определяем новый тип PostOrder для post-order обхода
newtype PostOrder a = Post (Tree a)

instance Show a => Show (PostOrder a) where
    show (Post tree) = postOrderShow tree
      where
        postOrderShow (Node val [])       = show val
        postOrderShow (Node val subtrees) = postOrderSubtrees subtrees ++ show val
        postOrderSubtrees []              = ""
        postOrderSubtrees (t:ts)          = postOrderShow t ++ postOrderSubtrees ts

---------------------------------------

-- 2.b Напишите инстанс `Eq` для дерева (0,5 балла)

instance (Eq a) => Eq (Tree a) where
  (Node val1 children1) == (Node val2 children2) =
    val1 == val2 && children1 == children2

------------------------------------------------------------------------------------------------

-- 3. Цвета (2 балла)

-- | Зададим тип данных RGB, но так как его поля должны быть от 0 до 255, назовем конструктор Unsafe...
--
data RGB = UnsafeMkRGB
  { red   :: Int
  , green :: Int
  , blue  :: Int
  } deriving (Show, Eq)

-- | ...и зададим новый конструктор, который будет проверять значения полей при инициализации
--
mkRGB :: Int -> Int -> Int -> Maybe RGB
mkRGB r g b
  | inRange (0, 255) `all` [r, g, b] = Just $ UnsafeMkRGB r g b
  | otherwise                                 = Nothing 

-- | Аналогично поступим, задавая тип данных CMYK
--
data CMYK = UnsafeMkCMYK
  { cyan    :: Int
  , magenta :: Int
  , yellow  :: Int
  , black   :: Int
  } deriving (Show, Eq)

mkCMYK :: Int -> Int -> Int -> Int -> Maybe CMYK
mkCMYK c m y b
  | inRange (0, 100) `all` [c, m, y, b] = Just $ UnsafeMkCMYK c m y b
  | otherwise                                             = Nothing 

---------------------------------------
-- 3.a Напишите инстансы класса ToCMYK для [Int] и для RGB (0,75 балла)

class ToCMYK a where
    toCMYK :: a -> Maybe CMYK


-- Инстанс ToCMYK для [Int]
instance ToCMYK [Int] where
    toCMYK [c, m, y, b]
        | inRange (0, 100) `all` [c, m, y, b] = Just $ UnsafeMkCMYK (fromIntegral c) (fromIntegral m) (fromIntegral y) (fromIntegral b)
        | otherwise = Nothing
    toCMYK _ = Nothing  -- Добавили несопоставленный паттерн


-- Инстанс ToCMYK для [RGB]
instance ToCMYK RGB where
  toCMYK (UnsafeMkRGB {red=r, green=g, blue=b}) = mkCMYK c m y (round (k * 100 :: Double)) 
    where
      r' = fromIntegral r / 255
      g' = fromIntegral g / 255
      b' = fromIntegral b / 255
      k = 1 - maximum [r', g', b']
      c = if k == 1 then 0 else round ((1 - r' - k) / (1 - k) * 100)
      m = if k == 1 then 0 else round ((1 - g' - k) / (1 - k) * 100)
      y = if k == 1 then 0 else round ((1 - b' - k) / (1 - k) * 100)

---------------------------------------

-- 3.b Классы типов -- это функции (1 балл)

-- | задайте класс типов ToCMYK используя data (аналогично Eq' из практики)
--
newtype DToCMYK a = MkDToCMYK { toCMYK' :: a -> Maybe CMYK }

{-
Found:
  data DToCMYK a = MkDToCMYK {toCMYK' :: a -> Maybe CMYK}
Perhaps:
  newtype DToCMYK a = MkDToCMYK {toCMYK' :: a -> Maybe CMYK}
Note: decreases laziness

В задании написано чеерез Data
-}
-- | реализуйте класс типов DToCMYK для [Int] и для RGB в виде функций (аналогично dEqBool из практики)

-- Реализуем класс типов DToCMYK для [Int]
dToCMYKList :: DToCMYK [Int]
dToCMYKList = MkDToCMYK toCMYKList
  where
    toCMYKList [r, g, b]
        | inRange (0, 255) r && inRange (0, 255) g && inRange (0, 255) b = Just $ UnsafeMkCMYK c m y k
        | otherwise = Nothing
        where
            c = 100 - (r * 100 `div` 255)
            m = 100 - (g * 100 `div` 255)
            y = 100 - (b * 100 `div` 255)
            k = minimum [c, m, y]
    toCMYKList _ = Nothing  -- Добавили несопоставленный паттерн
-- Реализуем класс типов DToCMYK для RGB
dToCMYKRGB :: DToCMYK RGB
dToCMYKRGB = MkDToCMYK toCMYKRGB
  where
    toCMYKRGB (UnsafeMkRGB r g b)
        | inRange (0, 255) `all` [r, g, b] = Just $ UnsafeMkCMYK c m y k
        | otherwise = Nothing
        where
            c = 100 - (r * 100 `div` 255)
            m = 100 - (g * 100 `div` 255)
            y = 100 - (b * 100 `div` 255)
            k = minimum [c, m, y]
---------------------------------------

-- 3.c Используйте инстансы (0,25 балла)
--     Приведите пример использования инстансов [Int] и RGB реализованных в 3a и 3b (должно получится 4 примера)

-- Определение цвета "someColor" с использованием списка [Int]
someColor :: [Int]
someColor = [0, 100, 10, 20]

-- Пример 1: Преобразование цвета "someColor" из списка [Int] в CMYK с использованием инстанса
someColorFromIntToCMYK :: Maybe CMYK
someColorFromIntToCMYK = toCMYK someColor

-- Пример 2: Преобразование цвета "someColor" из списка [Int] в CMYK с использованием класса типов
someColorFromIntToCMYK' :: Maybe CMYK
someColorFromIntToCMYK' = toCMYK' dToCMYKList someColor

-- Определение цвета "someColor" с использованием RGB
someColorRGB :: RGB
someColorRGB = UnsafeMkRGB 204 0 184

-- Пример 3: Преобразование цвета "someColor" из RGB в CMYK с использованием инстанса
someColorFromRGBToCMYK :: Maybe CMYK
someColorFromRGBToCMYK = toCMYK someColorRGB

-- Пример 4: Преобразование цвета "someColor" из RGB в CMYK с использованием класса типов
someColorFromRGBToCMYK' :: Maybe CMYK
someColorFromRGBToCMYK' = toCMYK' dToCMYKRGB someColorRGB

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

{-
pointful a b c d = a b (c d)
pointful a b c d = a b $ c d -- заменяем скобки на $
Последнюю строку можно переписать используя композицию
pointful a b c d = (a b . c) d
Теперь можно использовать $ 
pointful a b c d = a b . c $ d 
Вот теперь удаляем d (eta-reduction)
pointful a b c d = a b . c
Теперь используем оператор композиции как в примере из лекции
pointful a b c   = (.) (a b) c
Можем убрать с (eta-reduction)
pointful a b c   = (.) (a b) 
Заменяем скобки на $
pointful a b c   = (.) $ a b
Снова используем композицию
pointful a b c   = (.) . a $ b 
Можем убрать b (eta-reduction)
pointful a b c   = (.) . a 
Используем (.) как оператор
pointful a b c   = (.) (.) a
Можем убрать a (eta-reduction)
pointful a b c   = (.) (.)
-}
------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq)

instance Enum Day where
  toEnum n = case n `mod` 7 of
    0 -> Monday
    1 -> Tuesday
    2 -> Wednesday
    3 -> Thursday
    4 -> Friday
    5 -> Saturday
    6 -> Sunday
    _ -> error "Invalid day value"
  
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
nextDay :: Day -> Day
nextDay day = toEnum $ (fromEnum day + 1) `mod` 7

-- | Возвращает предыдущий день
dayBefore :: Day -> Day
dayBefore day = toEnum $ (fromEnum day - 1) `mod` 7

-- | Возвращает количество дней до ближайшей субботы
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
  deriving (Show, Eq)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева из задания 2 (0,5 балла)


instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node val childNode) = Node (f val) (map (fmap f) childNode)

---------------------------------------

-- 6.c(*) Реализуйте инстанс Functor для пары (1 балл)

data Pair a b = Pair a b
  deriving (Show, Eq)


instance Functor (Pair a) where
  fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair a b) = Pair a (f b)

-- С какими трудностями вы столкнулись?
-- Большие трудности возникают с тем, что cabal test ругается на изначальный код из домашки
-- и возникают ошибки компиляции со всем кодом из 6 и 7 заданий, но при :r все компилируется.

------------------------------------------------------------------------------------------------

-- 7. Класс типов `Bifunctor` (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Functor для Either и пары
-- Наверное, все таки имелось ввиду Bifunctor

-- Определение кастомного типа Either'
data Either' a b = Left' a | Right' b
  deriving (Show, Eq)

-- Инстанс Bifunctor для Either
instance Bifunctor Either' where
  bimap :: (a -> c) -> (b -> d) -> Either' a b -> Either' c d
  bimap f _ (Left' a)  = Left' (f a)
  bimap _ g (Right' b) = Right' (g b)

-- Инстанс Bifunctor для пары (Tuple)
instance Bifunctor Pair where
  bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
  bimap f g (Pair a b) = Pair (f a) (g b)

------------------------------------------------------------------------------------------------

-- 8. Задайте свой класс типов, опираясь на интересы, хобби и фазу Луны (0,25 балла)

class GumRats a where
    workout :: a -> String
    workoutPlan :: a -> String
    
instance GumRats Day where
    workout Monday    = "Chest workout on Monday"
    workout Tuesday   = "Back workout on Tuesday"
    workout Wednesday = "Shoulder workout on Wednesday"
    workout Thursday  = "Arm workout on Thursday"
    workout Friday    = "Leg workout on Friday"
    workout _         = "Rest on the weekend"

    workoutPlan Monday    = "1. Bench press\n2. Incline dumbbell press\n3. Dumbbell flyes"
    workoutPlan Tuesday   = "1. Deadlift\n2. Pull-ups\n3. Barbell rows"
    workoutPlan Wednesday = "1. Military press\n2. Lateral raises\n3. Front raises"
    workoutPlan Thursday  = "1. Bicep curls\n2. Tricep dips\n3. Hammer curls"
    workoutPlan Friday    = "1. Squats\n2. Lunges\n3. Leg press"
    workoutPlan _         = "Rest on the weekend"
------------------------------------------------------------------------------------------------
