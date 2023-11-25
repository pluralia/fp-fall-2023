import Data.Ix (Ix, inRange, index, range)
import Data.Time (DayOfWeek (Friday, Saturday, Thursday, Wednesday))

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

instance Num ChurchNumber where
  (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (+) chN1 Zero = chN1
  (+) Zero chN2 = chN2
  (+) chN1 (Succ chN2) = Succ (chN1 + chN2)

  (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (*) a (Succ b) = (+) a (a * b)
  (*) a Zero = Zero

  (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  (-) Zero Zero = Zero
  -- можно реализовывать через Maybe, но решил оставить так;
  (-) Zero _ = Zero
  (-) (Succ a) (Succ b) = a - b

  negate _ = Zero
  abs m = m

  signum Zero = Zero
  signum _ = Succ Zero

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

instance Ix ChurchNumber where
  range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
  range (chN1, chN2) = helper chN1 chN2 []
    where
      helper chN1 chN2 acc = if chN1 == chN2 then acc ++ [chN2] else helper (Succ chN1) chN2 (acc ++ [chN1])

  index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
  index (chN1, chN2) chN3 = helper (chN1, chN2) chN3 0
    where
      helper (chN1, chN2) chN3 acc = if chN1 == chN3 then acc else helper (Succ chN1, chN2) chN3 (acc + 1)

  inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool
  inRange (chN1, chN2) chN3 = helper (chN1, chN2) chN3
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
(*&) a Zero = Zero

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd a (Succ b) = chAdd (Succ a) b
chAdd a churchZero = a

chSucc :: ChurchNumber -> ChurchNumber
chSucc = Succ

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult a (Succ b) = chAdd a (a `chMult` b)
chMult a Zero = Zero

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

churchIntCalcProd :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntCalcProd chN1 chN2 = helper (churchNullify chN1) (churchNullify chN2)
  where
    -- helper chN1 chN2 = chN2
    helper (_, _) (Zero, Zero) = (Zero, Zero)
    helper (Zero, Zero) (_, _) = (Zero, Zero)
    helper (Zero, b) (c, Zero) = (Zero, b *& c)
    helper (a, Zero) (c, Zero) = (a *& c, Zero)
    helper (a, Zero) (Zero, d) = (Zero, a *& d)
    helper (Zero, b) (Zero, d) = (b *& d, Zero)

churchIntAbs :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntAbs chN = helper (churchNullify chN)
  where
    helper (Zero, Zero) = (Zero, Zero)
    helper (a, Zero) = (a, Zero)
    helper (Zero, a) = (a, Zero)

churchIntSignum :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntSignum chN = helper (churchNullify chN)
  where
    helper (Zero, Zero) = (Zero, Zero)
    helper (a, Zero) = (Succ Zero, Zero)
    helper (Zero, a) = (Zero, Succ Zero)

fromInteger' :: Integer -> (ChurchNumber, ChurchNumber)
fromInteger' int = helper int (Zero, Zero)
  where
    helper 0 (a, b) = (a, b)
    helper int (a, b) = helper (int - 1) (Succ a, Zero)

churchIntNegation :: (ChurchNumber, ChurchNumber) -> (ChurchNumber, ChurchNumber)
churchIntNegation (pos1, neg1) = churchNullify (neg1, pos1)

churchIntIsPositive :: (ChurchNumber, ChurchNumber) -> Bool
churchIntIsPositive chN = helper (churchNullify chN)
  where
    helper (Zero, Zero) = False
    helper (_, Zero) = True
    helper (Zero, _) = False

churchIntIsZero :: (ChurchNumber, ChurchNumber) -> Bool
churchIntIsZero (Zero, Zero) = True
churchIntIsZero (_, _) = False

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)r

-- pointful :: (t1 -> t2 -> t3) -> t1 -> (t4 -> t2) -> t4 -> t3
-- pointful a b c d = a b (c d)
-- pointful a b c d = a b $ (c d) написали доллар
-- pointful a b c d = a b $ c d убрали скобки
-- pointful a b c d = a b $ c убрали d
-- pointful a b c d = a b $ убрали c
-- pointful a b c d = a b убрали $
-- pointful a b c d = a убрали b
-- pointful a b c d = убрали a
-- pointful a b c d = ???
-- pointful a b c d = (.)

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
  toEnum :: Int -> Day
  toEnum num
    | num == 0 = Monday'
    | num == 1 = Tuesday'
    | num == 2 = Wednesday'
    | num == 3 = Thursday'
    | num == 4 = Friday'
    | num == 5 = Saturday'
    | num == 6 = Sunday'

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
daysBeforeWeekend day = helper day 0
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
