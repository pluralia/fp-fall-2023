import Data.Ix (
                  inRange
                , Ix
                , range
                , index
                , inRange
               )
import Data.Bifunctor ( Bifunctor(bimap) ) 

------------------------------------------------------------------------------------------------

-- 1. Числа Черча (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча

data ChurchNumber = Zero | Succ ChurchNumber
  deriving Show

instance Eq ChurchNumber where
  (==) :: ChurchNumber -> ChurchNumber -> Bool
  Zero   == Zero   = True
  Succ a == Succ b = a == b
  _      == _      = False
  
instance Ord ChurchNumber where
  compare :: ChurchNumber -> ChurchNumber -> Ordering
  compare Zero Zero         = EQ
  compare Zero _            = LT
  compare _ Zero            = GT
  compare (Succ a) (Succ b) = compare a b

instance Num ChurchNumber where
  (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (+) a Zero     = a
  (+) a (Succ b) = Succ (a + b)

  (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (-) a Zero            = a
  (-) Zero a            = error "There are no negative Church numbers"
  (-) (Succ a) (Succ b) = a - b

  (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (*) _ Zero     = Zero
  (*) a (Succ b) = a + (a * b)

  abs :: ChurchNumber -> ChurchNumber
  abs a = a

  signum :: ChurchNumber -> ChurchNumber
  signum Zero = Zero
  signum _    = Succ Zero

  fromInteger :: Integer -> ChurchNumber
  fromInteger n | n == 0    = Zero
                | n < 0     = error "There are no negative Church numbers"
                | otherwise = Succ (fromInteger (n - 1))

instance Enum ChurchNumber where
  toEnum :: Int -> ChurchNumber
  toEnum 0 = Zero
  toEnum n = Succ (toEnum (n - 1))

  fromEnum :: ChurchNumber -> Int
  fromEnum Zero     = 0
  fromEnum (Succ n) = 1 + fromEnum n
              
instance Ix ChurchNumber where
  range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
  range (a, b) = [a..b]

  index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
  index (a, _) b = fromEnum (b - a)

  inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool
  inRange (a, b) c = c >= a && c <= b

-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

-- pointful :: (t1 -> t2 -> t3) -> t1 -> (t4 -> t2) -> t4 -> t3
-- pointful a b c d = a b (c d) 
-- pointful a b c d = (a b . c) d -- убираю явный аргумент
-- pointful a b c = (a b . c) = a (.) b c -- использую оператор композиции
-- pointful a = (a .) -- с помощью опрератора композиции избавляюсь от еще двух аргументов
-- pointful = (.)
------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show)

instance Enum Day where
  fromEnum :: Day -> Int
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

  toEnum :: Int -> Day
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
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
daysBeforeWeekend Sunday = 6
daysBeforeWeekend d = fromEnum Saturday - fromEnum d

------------------------------------------------------------------------------------------------

-- 6. Класс типов `Functor` (1,25 балла)

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
  fmap f (Cons a rest) = Cons (f a) (fmap f rest)

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева (0,5 балла)

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  }

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node val children) = Node (f val) (map (fmap f) children)

---------------------------------------

-- 6.c Реализуйте инстанс Functor для пары (0,5 балл)

data Pair a b = Pair a b
  deriving (Show)

instance Functor (Pair a) where
  fmap :: (a2 -> b) -> Pair a1 a2 -> Pair a1 b
  fmap f (Pair a b) = Pair a (f b)

-- С какими трудностями вы столкнулись?
-- Что f применяется только ко второму аргументу

------------------------------------------------------------------------------------------------

-- 7. Класс типов Bifunctor (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Bifunctor для Either и пары

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
  bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
  bimap f _ (Left' a) = Left' (f a)
  bimap _ g (Right' b) = Right' (g b)

instance Bifunctor Pair where
  bimap :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
  bimap f g (Pair a b) = Pair (f a) (g b)

------------------------------------------------------------------------------------------------
