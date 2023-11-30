{-# LANGUAGE InstanceSigs #-}

module MyLib where
import Data.Ix 
import Data.Bifunctor ( Bifunctor(bimap) ) 
------------------------------------------------------------------------------------------------

-- 1. Числа Черча (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

instance Eq ChurchNumber where
    (==) :: ChurchNumber -> ChurchNumber -> Bool
    (==) Zero Zero = True
    (==) Zero _    = False
    (==) _ Zero    = False
    (==) (Succ a) (Succ b) = a == b




instance Ord ChurchNumber where
    compare :: ChurchNumber -> ChurchNumber -> Ordering
    compare Zero Zero = EQ
    compare Zero _    = LT
    compare _ Zero    = GT
    compare (Succ a) (Succ b) = compare a b



instance Num ChurchNumber where
    abs a = a
    fromInteger n
        | n == 0    = Zero
        | n < 0     = error "There are no negative Church numbers"
        | otherwise = Succ (fromInteger (n - 1))
    signum Zero = Zero
    signum _    = Succ Zero

    (+) x Zero = x
    (+) x (Succ y) = (+) (Succ x) y

    (-) x Zero = x
    (-) Zero _ = Zero 
    (-) (Succ x) (Succ y) = (-) x y

    (*) _ Zero = Zero
    (*) x (Succ y) = (+) x ((*) x y)




instance Enum ChurchNumber where
    toEnum n
        | n < 0     = error "Invalid"
        | n == 0    = Zero
        | otherwise = Succ (toEnum (n - 1))

    fromEnum Zero     = 0
    fromEnum (Succ n) = 1 + fromEnum n
             
instance Ix ChurchNumber where
    range (a, b)
        | a > b     = []
        | otherwise = take (fromEnum (b - a) + 1) $ iterate (\x -> Succ x) a

    index (a, _) b = fromEnum (b - a)

    inRange (a, b) c = c >= a && c <= b


-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)
pointful :: (a -> b -> c) -> a -> (d -> b) -> d -> c
pointful f x g y = f x (g y)

-- pointful f x g y = f x (g y)
-- pointful f x g = f x . g
-- pointful f x = (f x .)
-- pointful f = (f .)
-- pointful = (.)


------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq)

instance Enum Day where
  fromEnum Monday    = 1
  fromEnum Tuesday   = 2
  fromEnum Wednesday = 3
  fromEnum Thursday  = 4
  fromEnum Friday    = 5
  fromEnum Saturday  = 6
  fromEnum Sunday    = 7

  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday
  toEnum _ = error "Invalid day number"
---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
--
nextDay :: Day -> Day
nextDay Sunday = Monday
nextDay d = succ d

-- | Возвращает предыдущий день
--
dayBefore :: Day -> Day
dayBefore Monday = Sunday
dayBefore d = pred d

-- | Возвращает количество от текущего до ближайшей субботы
--
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend d = if d == Sunday
                      then 6
                      else fromEnum Saturday - fromEnum d
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
  fmap _ Nil           = Nil
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева (0,5 балла)

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  }
  deriving (Show, Eq)

instance Functor Tree where
    fmap f (Node val subtrees) = Node (f val) (map (fmap f) subtrees)

---------------------------------------

-- 6.c Реализуйте инстанс Functor для пары (0,5 балл)

data Pair a b = Pair a b
  deriving (Show,Eq)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)
-- С какими трудностями вы столкнулись?
------------------------------------------------------------------------------------------------

-- 7. Класс типов Bifunctor (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Bifunctor для Either и пары
data Either' a b = Left' a | Right' b
  deriving (Show,Eq)

instance Functor (Either' a) where
    fmap :: (b -> c) -> Either' a b -> Either' a c
    fmap _ (Left' a) = Left' a
    fmap f (Right' b) = Right' (f b)

instance Bifunctor Either' where
    bimap :: (a -> c) -> (b -> d) -> Either' a b -> Either' c d
    bimap f _ (Left' a)  = Left' (f a)
    bimap _ g (Right' b) = Right' (g b)

------------------------------------------------------------------------------------------------

