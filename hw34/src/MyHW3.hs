{-# LANGUAGE InstanceSigs, OverloadedStrings #-}
module MyHW3 where

import Data.Ix (inRange, Ix, range, index)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.List as L (foldl')

------------------------------------------------------------------------------------------------

-- 1. Числа Черча (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

instance Eq ChurchNumber where
  (==) :: ChurchNumber -> ChurchNumber -> Bool
  Zero == Zero         = True
  Zero == _            = False
  _ == Zero            = False
  (Succ m) == (Succ n) = n == m

instance Ord ChurchNumber where
  (<=) :: ChurchNumber -> ChurchNumber -> Bool
  Zero <= _            = True
  _    <= Zero         = False
  (Succ m) <= (Succ n) = m <= n

instance Num ChurchNumber where
  (+), (*), (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  abs, signum :: ChurchNumber -> ChurchNumber
  fromInteger :: Integer -> ChurchNumber

  Zero + m             = m
  (Succ n) + m         = Succ (n + m)

  m - Zero             = m
  Zero - _             = Zero
  (Succ n) - (Succ m)  = n - m

  Zero * _             = Zero
  _ * Zero             = Zero
  (Succ m) * n         = n + m * n

  abs m                = m

  signum Zero          = Zero
  signum _             = Succ Zero

  fromInteger 0        = Zero
  fromInteger n        = 
    if n > 0
      then Succ (fromInteger (n - 1))
      else Zero


-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

instance Ix ChurchNumber where
  range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
  index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
  inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool

  range = helper []
    where 
      helper :: [ChurchNumber] -> (ChurchNumber, ChurchNumber) -> [ChurchNumber]
      helper lst (l, r) | l > r      = error "Lower bound must be less then upper bound!"
                        | l == r     = r : lst
                        | otherwise  = helper (r : lst) (l, r - Succ Zero)
  
  index = helper 0
    where
      helper :: Int -> (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
      helper idx (l, r) num | l > r              = error "Lower bound must be less then upper bound!"
                            | num < l || num > r = error "This range doesn't contain your number!"
                            | l == num           = idx
                            | otherwise          = helper (succ idx) (Succ l, r) num
  
  inRange (l, r) num | l > r     = error "Lower bound must be less then upper bound!"
                     | otherwise = l <= num && num <= r


------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

pointful :: (t1 -> t2 -> t3) -> t1 -> (t4 -> t2) -> t4 -> t3
pointful a b c d = a b (c d)
-- p f x g y = f x (g y)      - записали функцию
-- p f x g y = f x $ g y      - убрали скобки
-- p f x g y = (f x) $ g y    - теперь (f x) - функция
-- p f x g y = (f x) . g $ y  - композиция двух функций: (f x) и g
-- p f x g   = (f x) . g      - редукция
-- p f x g   = ((.) (f x)) g  - записали в операторном стиле
-- p f x     = (.) (f x)      - редукция
-- p f x     = (.) $ f x      - убрали скобки
-- p f x     = (.) . f $ x    - композиция двух функций: (.) и f
-- p f       = (.) . f        - редукция
-- p f       = (.) (.) f      - записали в операторном стиле
-- p         = (.) (.)        - редукция, готово


------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Monday | Tuesday | Wednesday |
           Thursday | Friday | Saturday | Sunday
  deriving(Show, Eq)
  
instance Enum Day where
  toEnum :: Int -> Day
  fromEnum :: Day -> Int

  toEnum n | n == 1    = Monday
           | n == 2    = Tuesday
           | n == 3    = Wednesday
           | n == 4    = Thursday
           | n == 5    = Friday
           | n == 6    = Saturday
           | n == 7    = Sunday
           | otherwise = error "No match!"

  fromEnum d | d == Monday    = 1
             | d == Tuesday   = 2
             | d == Wednesday = 3
             | d == Thursday  = 4
             | d == Friday    = 5
             | d == Saturday  = 6
             | d == Sunday    = 7
             | otherwise = error "Such a day does not exist!"

---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
--
nextDay :: Day -> Day
nextDay d = toEnum $ calcDay d
  where
   calcDay :: Day -> Int
   calcDay day = if day == Sunday then 1 else succ $ fromEnum day


-- | Возвращает предыдущий день
--
dayBefore :: Day -> Day
dayBefore d = toEnum $ calcDay d 
  where
   calcDay :: Day -> Int
   calcDay day = if day == Monday then 7 else pred $ fromEnum day

-- | Возвращает количество от текущего до ближайшей субботы
--
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend Saturday = 0
daysBeforeWeekend day      = succ $ daysBeforeWeekend $ nextDay day

------------------------------------------------------------------------------------------------

-- 6. Класс типов `Functor` (1,25 балла)

-- Вы уже знакомы с функцией `map` -- мы писали ее для различных типов данных (mapMaybe, mapList)
-- На самом деле она задана в классе типов `Functor`
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Functor.html

---------------------------------------

-- 6.a Реализуйте инстанс Functor для списка (0,25 балла)

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil          = Nil
  fmap f (Cons a lst) = Cons (f a) (fmap f lst)
---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева (0,5 балла)

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  }
  deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node v c) = Node {value = f v
                           , children = reverse (L.foldl' (\acc x -> fmap f x : acc) [] c)
                           } 

---------------------------------------

-- 6.c Реализуйте инстанс Functor для пары (0,5 балл)

data Pair a b = Pair a b
  deriving (Show, Eq)

instance Functor (Pair a) where
  fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair x y) = Pair x $ f y  


-- С какими трудностями вы столкнулись?
-- Что fmap может изменять только последний параметр, если конструктор типа включает в себя несколько параметров

------------------------------------------------------------------------------------------------

-- 7. Класс типов Bifunctor (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Bifunctor для Either и пары

data Either' a b = Left' a | Right' b 
  deriving(Show, Eq)

instance Functor (Either' a) where 
  fmap :: (b -> c) -> Either' a b -> Either' a c
  fmap _ (Left' x)  = Left' x
  fmap f (Right' x) = Right' (f x)

instance Bifunctor Either' where
  bimap :: (a -> c) -> (b -> d) -> Either' a b -> Either' c d
  bimap f _ (Left' x)  = Left' $ f x
  bimap _ g (Right' y) = Right' $ g y 

instance Bifunctor Pair where
  bimap :: (a -> c) -> (b -> d) -> Pair a b -> Pair c d
  bimap f g (Pair x y) = Pair (f x) (g y)
