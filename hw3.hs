import Data.Ix
import Data.List qualified as L (foldl')

-- testAll :: Bool
-- testAll =
--   and
--     [ testChurch1
--     ]

------------------------------------------------------------------------------------------------

-- 1. Числа Черча и `Ix` (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

-- (Succ Zero)

-- newtype ChurchIntNumber = Zero Zero | Succ
--   deriving (Show)

-- (Succ Zero, Succ Zero)

-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

compareChurch :: ChurchNumber -> ChurchNumber
compareChurch Zero = Zero
compareChurch (Succ c) = compareChurch c

instance Eq ChurchNumber where
  (==) :: ChurchNumber -> ChurchNumber -> Bool
  Succ a == Succ b = a == b
  Zero == Zero = True
  _ == _ = False

instance Ord ChurchNumber where
  (<=) :: ChurchNumber -> ChurchNumber -> Bool
  Succ a <= Succ b = a <= b
  Succ _ <= Zero = False
  Zero <= Succ _ = True
  Zero <= Zero = True

-- necessary funcs
chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd a (Succ b) = chAdd (Succ a) b
chAdd a churchZero = a

chSucc :: ChurchNumber -> ChurchNumber
chSucc = Succ

-- for testing purposes
churchZero :: ChurchNumber
churchZero = Zero

churchOne :: ChurchNumber
churchOne = chSucc churchZero

churchTwo :: ChurchNumber
churchTwo = chSucc churchOne

churchThree :: ChurchNumber
churchThree = chSucc churchTwo

churchIntZero :: (ChurchNumber, ChurchNumber)
churchIntZero = (churchZero, churchZero)

churchIntOne :: (ChurchNumber, ChurchNumber)
churchIntOne = (churchOne, Zero)

churchIntOneNeg :: (ChurchNumber, ChurchNumber)
churchIntOneNeg = churchIntNegation churchIntOne

churchIntTwo :: (ChurchNumber, ChurchNumber)
churchIntTwo = churchIntCalcSum churchIntOne churchIntOne

churchIntTwoNeg :: (ChurchNumber, ChurchNumber)
churchIntTwoNeg = churchIntCalcSum churchIntOneNeg churchIntOneNeg

churchIntThree :: (ChurchNumber, ChurchNumber)
churchIntThree = churchIntCalcSum churchIntTwo churchIntOne

churchIntThreeNeg :: (ChurchNumber, ChurchNumber)
churchIntThreeNeg = churchIntCalcSum churchIntTwoNeg churchIntOneNeg

(+&) :: ChurchNumber -> ChurchNumber -> ChurchNumber
(+&) a (Succ b) = (+&) (Succ a) b
(+&) a Zero = a

(*&) :: ChurchNumber -> ChurchNumber -> ChurchNumber
(*&) a (Succ b) = (+&) a (a *& b)
(*&) a Zero = Zero

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

-- negative / positive
-- (+), (*), abs, signum, fromInteger, (negate | (-))

-- instance Num ChurchIntNumber where
--   (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
--   (+) a (Succ b) = (+) (Succ a) b
--   (+) a churchZero = a

--   (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
--   (*) a (Succ b) = (*) a ((*) a b)
--   (*) a Zero = Succ Zero

tC1 :: ChurchNumber -> ChurchNumber -> Bool
tC1 = (==)

tC2 :: ChurchNumber -> ChurchNumber -> Bool
tC2 = (<=)

-- The Ix class is used to map a contiguous subrange of values in type onto integers.
-- It is used primarily for array indexing (see the array package). Ix uses row-major order.

-- The Ix class
-- range, (index | unsafeIndex), inRange

-- range :: (a, a) -> [a] Source #
-- The list of values in the subrange defined by a bounding pair.

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

  rangeSize :: (ChurchNumber, ChurchNumber) -> Int
  rangeSize (chN1, chN2) = helper (chN1, chN2) 0
    where
      helper (chN1, chN2) acc = if chN1 == chN2 then acc else helper (Succ chN1, chN2) (acc + 1)

------------------------------------------------------------------------------------------------

-- 2. Дерево (2 балла)

data Tree a
  = Leaf
  | Node
      { value :: a,
        children :: [Tree a]
      }

-- :p

dfs :: Tree a -> [a]
dfs Leaf = []
dfs (Node val ch) = val : L.foldl' (\acc node -> acc ++ dfs node) [] ch

---------------------------------------

-- 2.a Напишите 3 инстанса `Show` для дерева.
--     Истансы должны реализовывать in-, pre-, post-order обход дерева (1,5 балла)

-- Мы не можем определять несколько инстансов одного типа для одного класса,
-- поэтому обернем наш тип `Tree` в newtype, чтобы у нас было 3 разных типа,
-- и уже для них реализуем `Show`

-- how to convert
-- post-order -> dfs on exit
-- pre-order -> dfs on entrance
-- inorder -> dfs after in-out

-- | in-order обход дерева
newtype InOrder a = In (Tree a)

instance (Show a) => Show (InOrder a) where
  show :: (Show a) => InOrder a -> String
  show = undefined

-- | pre-order обход дерева
newtype PreOrder a = Pre (Tree a)

instance (Show a) => Show (PreOrder a) where
  show :: (Show a) => PreOrder a -> String
  show = undefined

-- | post-order обход дерева
newtype PostOrder a = Post (Tree a)

instance (Show a) => Show (PostOrder a) where
  show :: (Show a) => PostOrder a -> String
  show = undefined

---------------------------------------

-- 2.b Напишите инстанс `Eq` для дерева (0,5 балла)

------------------------------------------------------------------------------------------------

-- 3. Цвета (2 балла)

-- | Зададим тип данных RGB, но так как его поля должны быть от 0 до 255, назовем конструктор Unsafe...
data RGB = UnsafeMkRGB
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

-- | ...и зададим новый конструктор, который будет проверять значения полей при инициализации
mkRGB :: Int -> Int -> Int -> Maybe RGB
mkRGB red green blue
  | inRange (0, 255) `all` [red, green, blue] = Just $ UnsafeMkRGB red green blue
  | otherwise = Nothing

-- | Аналогично поступим, задавая тип данных CMYK
data CMYK = UnsafeMkCMYK
  { cyan :: Int,
    magenta :: Int,
    yellow :: Int,
    black :: Int
  }
  deriving (Show)

mkCMYK :: Int -> Int -> Int -> Int -> Maybe CMYK
mkCMYK cyan magenta yellow black
  | inRange (0, 100) `all` [cyan, magenta, yellow, black] = Just $ UnsafeMkCMYK cyan magenta yellow black
  | otherwise = Nothing

---------------------------------------

-- 3.a Напишите инстансы класса ToCMYK для [Int] и для RGB (0,75 балла)

class ToCMYK a where
  toCMYK :: a -> Maybe CMYK

---------------------------------------

-- 3.b Классы типов -- это функции (1 балл)

-- | задайте класс типов ToCMYK используя data (аналогично Eq' из практики)
data DToCMYK = Placeholder

-- | реализуйте класс типов DToCMYK для [Int] и для RGB в виде функций (аналогично dEqBool из практики)

---------------------------------------

-- 3.c Используйте инстансы (0,25 балла)
--     Приведите пример использования инстансов [Int] и RGB реализованных в 3a и 3b (должно получится 4 примера)

------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

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
  fmap = undefined

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева из задания 2 (0,5 балла)

---------------------------------------

-- 6.c(*) Реализуйте инстанс Functor для пары (1 балл)

data Pair a b = Pair a b
  deriving (Show)

-- С какими трудностями вы столкнулись?

------------------------------------------------------------------------------------------------

-- 7. Класс типов Bifunctor (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Bifunctor для Either и пары

------------------------------------------------------------------------------------------------

-- 8. Задайте свой класс типов, опираясь на интересы, хобби и фазу Луны (0,25 балла)

------------------------------------------------------------------------------------------------
