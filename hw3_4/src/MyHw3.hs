import Data.Ix (inRange)

------------------------------------------------------------------------------------------------

-- 1. Числа Черча (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

-- for tests
churchZero = Zero
churchOne = Succ Zero
churchTwo = Succ (Succ Zero)

instance Eq ChurchNumber where
  (==) :: ChurchNumber -> ChurchNumber -> Bool
  Zero == Zero = True
  Succ a == Succ b = a == b
  _ == _ = False

instance Ord ChurchNumber where
  (<=) :: ChurchNumber -> ChurchNumber -> Bool
  Succ a <= Succ b = a <= b
  Succ _ <= Zero = False
  Zero <= Succ _ = True

instance Num ChurchNumber where
  (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (+) chN1 Zero = chN1
  (+) Zero chN2 = chN2
  (+) chN1 (Succ chN2) = Succ (chN1 + chN2)

  (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (*) chN1 Zero = Zero
-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)r

pointful :: (t1 -> t2 -> t3) -> t1 -> (t4 -> t2) -> t4 -> t3
pointful a b c d = a b (c d)

------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = MyDaysAreHere

---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
nextDay :: Day -> Day
nextDay = undefined

-- | Возвращает предыдущий день
dayBefore :: Day -> Day
dayBefore = undefined

-- | Возвращает количество от текущего до ближайшей субботы
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend = undefined

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
  fmap = undefined

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева (0,5 балла)

data Tree a = Node
  { value :: a,
    children :: [Tree a]
  }

---------------------------------------

-- 6.c Реализуйте инстанс Functor для пары (0,5 балл)

data Pair a b = Pair a b
  deriving (Show)

-- С какими трудностями вы столкнулись?

------------------------------------------------------------------------------------------------

-- 7. Класс типов Bifunctor (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Bifunctor для Either и пары

------------------------------------------------------------------------------------------------
