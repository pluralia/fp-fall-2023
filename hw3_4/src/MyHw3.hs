{-# LANGUAGE InstanceSigs #-}

module MyHw3 where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Ix (Ix, inRange, index, range)
import Data.Time ()

------------------------------------------------------------------------------------------------

-- 1. Числа Черча (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча

--

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

-- for tests

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
  Zero <= Zero = True

instance Num ChurchNumber where
  (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (+) chN1 Zero = chN1
  (+) Zero chN2 = chN2
  (+) chN1 (Succ chN2) = Succ (chN1 + chN2)

  (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (*) a (Succ b) = (+) a (a * b)
  (*) _ Zero = Zero

  (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  -- можно реализовывать через Maybe, но решил оставить так;
  (-) Zero _ = Zero
  (-) (Succ a) (Succ b) = a - b
  (-) (Succ a) Zero = Succ a

  -- комплиятор говорит, что non-exhaustive pattern matching
  -- линтер говорит, что pattern match is redundant ; D

  negate :: ChurchNumber -> ChurchNumber
  negate _ = Zero
  abs :: ChurchNumber -> ChurchNumber
  abs m = m

  signum :: ChurchNumber -> ChurchNumber
  signum Zero = Zero
  signum _ = Succ Zero

  fromInteger :: Integer -> ChurchNumber
  fromInteger 0 = Zero
  fromInteger n =
    if n > 0
      then Succ (fromInteger (n - 1))
      else Zero

churchZero :: ChurchNumber
churchZero = Zero

churchOne :: ChurchNumber
churchOne = Succ Zero

churchTwo :: ChurchNumber
churchTwo = Succ (Succ Zero)

churchThree :: ChurchNumber
churchThree = churchOne + churchTwo

churchFour :: ChurchNumber
churchFour = churchTwo * churchTwo

-- А можешь подсказать, пожалуйста, как здесь с shadowing бороться?
instance Ix ChurchNumber where
  range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
  range (chN11, chN22) = helper chN11 chN22 []
    where
      helper chN1 chN2 acc = if chN1 == chN2 then acc ++ [chN2] else helper (Succ chN1) chN2 (acc ++ [chN1])

  index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
  index (chN11, chN22) chN33 = helper (chN11, chN22) chN33 0
    where
      helper (chN1, chN2) chN3 acc = if chN1 == chN3 then acc else helper (Succ chN1, chN2) chN3 (acc + 1)

  inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool
  inRange (chN11, chN22) = helper (chN11, chN22)
    where
      helper (chN1, chN2) chN3 | chN1 == chN3 = True | chN1 == chN2 = False | otherwise = helper (Succ chN1, chN2) chN3

-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

-- Я немножечко упоролся и написал Num для негативных чисел черча, где число представлено как
-- (churchNumber, churcNumber), где
-- (churchOne, Zero) отвечает положительному churchOne; (Zero, churchOne) - отрицательному churchOne
-- проверять не надо, просто жалко было не коммитить :)

(+&) :: ChurchNumber -> ChurchNumber -> ChurchNumber
(+&) a (Succ b) = (+&) (Succ a) b
(+&) a Zero = a

(*&) :: ChurchNumber -> ChurchNumber -> ChurchNumber
(*&) a (Succ b) = (+&) a (a *& b)
(*&) _ Zero = Zero

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd a (Succ b) = chAdd (Succ a) b
chAdd a _ = a

chSucc :: ChurchNumber -> ChurchNumber
chSucc = Succ

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult a (Succ b) = chAdd a (a `chMult` b)
chMult _ Zero = Zero

churchNullify :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchNullify (Zero, Zero) = (Zero, Zero)
churchNullify (pos, Zero) = (pos, Zero)
churchNullify (Zero, neg) = (Zero, neg)
churchNullify (Succ pos, Succ neg) = churchNullify (pos, neg)

-- (pos1 + neg2), (pos2 + neg1)
churchIntCalcDifference :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntCalcDifference (pos1, neg1) (pos2, neg2) = churchNullify (pos1 +& neg2, neg1 +& pos2)

-- (pos1 + neg2), (pos2 + neg1)
churchIntCalcSum :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntCalcSum (pos1, neg1) (pos2, neg2) = churchNullify (pos1 +& pos2, neg1 +& neg2)

-- ругается на non-exhausitve pattern matching
-- churchIntCalcProd :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
-- churchIntCalcProd chN1 chN2 = helper (churchNullify chN1) (churchNullify chN2)
--   where
--     helper (_, _) (Zero, Zero) = (Zero, Zero)
--     helper (Zero, Zero) (_, _) = (Zero, Zero)
--     helper (Zero, b) (c, Zero) = (Zero, b *& c)
--     helper (a, Zero) (c, Zero) = (a *& c, Zero)
--     helper (a, Zero) (Zero, d) = (Zero, a *& d)
--     helper (Zero, b) (Zero, d) = (b *& d, Zero)

churchIntAbs :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntAbs chN = helper (churchNullify chN)
  where
    helper (Succ a, Succ b) = churchIntAbs (a, b)
    helper (Zero, Zero) = (Zero, Zero)
    helper (a, Zero) = (a, Zero)
    helper (Zero, a) = (a, Zero)

churchIntSignum :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntSignum chN = helper (churchNullify chN)
  where
    helper (Succ a, Succ b) = churchIntSignum (a, b)
    helper (Zero, Zero) = (Zero, Zero)
    helper (_, Zero) = (Succ Zero, Zero)
    helper (Zero, _) = (Zero, Succ Zero)

fromInteger' :: Integer -> (ChurchNumber, ChurchNumber)
fromInteger' int = helper int (Zero, Zero)
  where
    helper 0 (a, b) = (a, b)
    helper int1 (a, _) = helper (int1 - 1) (Succ a, Zero)

churchIntNegation :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntNegation (pos1, neg1) = churchNullify (neg1, pos1)

churchIntIsPositive :: (ChurchNumber, ChurchNumber) -> Bool
churchIntIsPositive chN = helper (churchNullify chN)
  where
    helper (Succ a, Succ b) = churchIntIsPositive (a, b)
    helper (Zero, Zero) = False
    helper (_, Zero) = True
    helper (Zero, _) = False

churchIntIsZero :: (ChurchNumber, ChurchNumber) -> Bool
churchIntIsZero (Zero, Zero) = True
churchIntIsZero (_, _) = False

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)r

-- pointful f x f' y = f x (f' y) = ((f x) . f') y -- можем переделать в композицию и использовать эта-редукцию
-- pointful f x f' = (f x) . f' -- результат
-- pointful f x f' = (.) (f x) f' -- преобразуем композицию в операторный стиль и снова используем эта редукцию
-- pointful f x = (.) (f x) -- результат
-- pointful f x = ((.) . f) x -- эта-редукция
-- pointful f = ((.) . f) -- результат
-- pointful f = ((.) .) f -- операторный стиль, применяем эта-редукцию
-- pointful = ((.) .) -- ответ
------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Sunday' | Monday' | Tuesday' | Wednesday' | Thursday' | Friday' | Saturday'
  deriving (Show, Eq)

instance Enum Day where
  fromEnum :: Day -> Int
  fromEnum day
    | day == Monday' = 0
    | day == Tuesday' = 1
    | day == Wednesday' = 2
    | day == Thursday' = 3
    | day == Friday' = 4
    | day == Saturday' = 5
    | day == Sunday' = 6
    | otherwise = -1

  toEnum :: Int -> Day
  toEnum num
    | num == 0 = Monday'
    | num == 1 = Tuesday'
    | num == 2 = Wednesday'
    | num == 3 = Thursday'
    | num == 4 = Friday'
    | num == 5 = Saturday'
    | num == 6 = Sunday'
    | otherwise = Saturday'

---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
nextDay :: Day -> Day
nextDay day = if day == Sunday' then Monday' else succ day

-- | Возвращает предыдущий день
dayBefore :: Day -> Day
dayBefore day = if day == Monday' then Sunday' else pred day

-- | Возвращает количество от текущего до ближайшей субботы
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend day1 = helper day1 0
  where
    helper day n = if day == Saturday' then n else helper (succ day) n + 1

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
  fmap _ Nil = Nil
  fmap func (Cons x xs) = Cons (func x) (fmap func xs)

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева (0,5 балла)

data Tree a = Node
  { value :: a,
    children :: [Tree a]
  } deriving (Show, Eq)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap func Node {value = val, children = ch} = Node {value = func val, children = map (fmap func) ch}

---------------------------------------

-- 6.c Реализуйте инстанс Functor для пары (0,5 балл)

data Pair a b = Pair a b
  deriving (Show, Eq)

instance Functor (Pair a) where
  fmap :: (a2 -> b) -> Pair a1 a2 -> Pair a1 b
  fmap f (Pair a b) = Pair a (f b)


------------------------------------------------------------------------------------------------

-- 7. Класс типов Bifunctor (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Bifunctor для Either и пары

data Either' a b = Left' a | Right' b
  deriving (Show, Eq)

instance Functor (Either' a) where
  fmap _ (Left' a) = Left' a
  fmap f (Right' b) = Right' (f b)

instance Bifunctor Either' where
  bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
  bimap f _ (Left' a)  = Left' (f a)
  bimap _ g (Right' b) = Right' (g b)

instance Bifunctor Pair where
  bimap :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
  bimap f g (Pair a b) = Pair (f a) (g b)
------------------------------------------------------------------------------------------------

