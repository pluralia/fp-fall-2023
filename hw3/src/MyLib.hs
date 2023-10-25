{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module MyLib where

import Data.Bifunctor
import Data.Ix
import Data.List as L
import Data.Set as S

-- default (Int, Double)

------------------------------------------------------------------------------------------------

-- 1. Числа Черча и `Ix` (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча
data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

instance Eq ChurchNumber where
  (==) :: ChurchNumber -> ChurchNumber -> Bool
  Zero == Zero = True
  Zero == _ = False
  _ == Zero = False
  Succ x == Succ y = x == y

instance Ord ChurchNumber where
  compare :: ChurchNumber -> ChurchNumber -> Ordering
  compare Zero Zero = EQ
  compare Zero (Succ _) = LT
  compare (Succ _) Zero = GT
  compare (Succ x) (Succ y) = compare x y

-- Возьмем реализованные функции из hw01
chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd x Zero = x
chAdd x (Succ y) = chAdd (Succ x) y

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult _ Zero = Zero
chMult x (Succ Zero) = x
chMult x (Succ y) = chAdd x (chMult x y)

chPrev :: ChurchNumber -> ChurchNumber
chPrev (Succ x) = x
chPrev Zero = Zero

instance Num ChurchNumber where
  (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  x + y = chAdd x y

  (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  x - Zero = x
  Zero - _ = Zero -- наверное это правильно. у нас ведь нет отрицательных чисел
  x - y = chPrev x - chPrev y

  (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
  x * y = chMult x y
  
  fromInteger :: Integer -> ChurchNumber
  fromInteger = helper Zero
    where
      helper :: ChurchNumber -> Integer -> ChurchNumber
      helper acc n
        | n <= 0 = acc
        | otherwise = helper (Succ acc) (n - 1)

  abs :: ChurchNumber -> ChurchNumber
  abs = id

  signum :: ChurchNumber -> ChurchNumber
  signum Zero = Zero
  signum _ = Succ Zero

instance Ix ChurchNumber where
  range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
  range = helper []
    where
      helper :: [ChurchNumber] -> (ChurchNumber, ChurchNumber) -> [ChurchNumber]
      helper acc (l, r)
        | l > r = acc
        | l == r = r : acc
        | otherwise = helper (r : acc) (l, chPrev r)
  -- calal пишет Pattern match(es) are non-exhaustive. Я вроде все учел

  inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool
  inRange (l, r) x = (x <= r) && (x >= l)

  index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
  index = helper 0
    where
      helper :: Int -> (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
      helper ind (l, r) x
        | x < l = -1
        | x > r = -1
        | x == l = ind
        | otherwise = helper (ind + 1) (l + Succ Zero, r) x

cn :: Integer -> ChurchNumber -- Чтобы было удобно дебажить
cn = fromInteger -- тк по дефолту fromInteger выдает Int

------------------------------------------------------------------------------------------------

-- 2. Дерево (2 балла)

data Tree a = Node
  { value :: a,
    children :: [Tree a]
  }
  deriving (Show)

---------------------------------------

-- 2.a Напишите 3 инстанса `Show` для дерева.
--     Истансы должны реализовывать in-, pre-, post-order обход дерева (1,5 балла)

-- Мы не можем определять несколько инстансов одного типа для одного класса,
-- поэтому обернем наш тип `Tree` в newtype, чтобы у нас было 3 разных типа,
-- и уже для них реализуем `Show`

-- | pre-order обход дерева
newtype PreOrder a = Pre (Tree a)

instance (Show a) => Show (PreOrder a) where
  show :: Show a => PreOrder a -> String
  show (Pre tree) = show (helper [tree])
    where
      helper :: [Tree a] -> [a]
      helper [] = []
      helper (node : nodes) = value node : helper (children node) ++ helper nodes

-- | in-order обход дерева
newtype InOrder a = In (Tree a)

instance (Show a) => Show (InOrder a) where
  show :: Show a => InOrder a -> String
  show (In tree) = show (helper [tree])
    where
      helper :: [Tree a] -> [a]
      helper [] = []
      helper (node : nodes)
        | L.null (children node) = value node : helper nodes
        | otherwise = (helper . L.singleton . head) (children node) ++ [value node] ++ (helper . tail) (children node) ++ helper nodes

-- | post-order обход дерева
newtype PostOrder a = Post (Tree a)

instance (Show a) => Show (PostOrder a) where
  show :: Show a => PostOrder a -> String
  show (Post tree) = show (helper [tree])
    where
      helper :: [Tree a] -> [a]
      helper [] = []
      helper (node : nodes) = helper (children node) ++ [value node] ++ helper nodes

---------------------------------------

-- 2.b Напишите инстанс `Eq` для дерева (0,5 балла)

instance (Ord a) => Ord (Tree a) where
  compare :: Ord a => Tree a -> Tree a -> Ordering
  compare x y = compare (value x) (value y)

instance (Ord a) => Eq (Tree a) where
  -- сделал так, что порядок children не важен (изоморфизм или типо того)
  -- Но S.fromList требует instance Ord (Tree a)
  -- А есть ли какие-то способы создания сета где не надо instance Ord?
  x == y = value x == value y && S.fromList (children x) == S.fromList (children y)

------------------------------------------------------------------------------------------------

-- 3. Цвета (2 балла)

-- | Зададим тип данных RGB, но так как его поля должны быть от 0 до 255, назовем конструктор Unsafe...
data RGB = UnsafeMkRGB
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show, Eq)

-- | ...и зададим новый конструктор, который будет проверять значения полей при инициализации
mkRGB :: Int -> Int -> Int -> Maybe RGB
mkRGB r g b
  | inRange (0, 255) `all` [r, g, b] = Just $ UnsafeMkRGB r g b
  | otherwise = Nothing

-- | Аналогично поступим, задавая тип данных CMYK
data CMYK = UnsafeMkCMYK
  { cyan :: Int,
    magenta :: Int,
    yellow :: Int,
    black :: Int
  }
  deriving (Show, Eq)

mkCMYK :: Int -> Int -> Int -> Int -> Maybe CMYK
mkCMYK c m y k
  | inRange (0, 100) `all` [c, m, y, k] = Just $ UnsafeMkCMYK c m y k
  | otherwise = Nothing

-- Мне в тестах нужна
unpack :: Maybe a -> a
unpack (Just x) = x
unpack Nothing = error "Input ins nothing" -- я понимаю что так делать не надо

---------------------------------------

-- 3.a Напишите инстансы класса ToCMYK для [Int] и для RGB (0,75 балла)

class ToCMYK a where
  toCMYK :: a -> Maybe CMYK

instance ToCMYK [Int] where
  toCMYK :: [Int] -> Maybe CMYK
  toCMYK [c, m, y, k]
    | inRange (0, 100) `all` [c, m, y, k] = Just $ UnsafeMkCMYK c m y k
    | otherwise = Nothing
  toCMYK _ = Nothing

instance ToCMYK RGB where
  toCMYK :: RGB -> Maybe CMYK
  toCMYK x =
    -- А можно как-то без явного объявления типов. Просто без них вылезают всякие warning
    let r' = (fromIntegral (red x) / 255.0) :: Double
        g' = (fromIntegral (green x) / 255.0) :: Double
        b' = (fromIntegral (blue x) / 255.0) :: Double
        k' = 1 - maximum [r', g', b']
        c = if k == 1 then 0 else (round $ (1.0 - r' - k') / (1.0 - k') * 100.0) :: Int
        m = if k == 1 then 0 else (round $ (1.0 - g' - k') / (1.0 - k') * 100.0) :: Int
        y = if k == 1 then 0 else (round $ (1.0 - b' - k') / (1.0 - k') * 100.0) :: Int
        k = (round $ k' * 100.0) :: Int
     in mkCMYK c m y k

---------------------------------------

-- 3.b Классы типов -- это функции (1 балл)

-- | задайте класс типов ToCMYK используя data (аналогично Eq' из практики)
newtype DToCMYK a = DMkCMYK
  { toCMYK' :: a -> Maybe CMYK
  }

dToCMYKfromList :: DToCMYK [Int]
dToCMYKfromList = DMkCMYK toCMYKfromList
  where
    toCMYKfromList :: [Int] -> Maybe CMYK
    toCMYKfromList [c, m, y, k]
      | inRange (0, 100) `all` [c, m, y, k] = Just $ UnsafeMkCMYK c m y k
      | otherwise = Nothing
    toCMYKfromList _ = Nothing

-- | реализуйте класс типов DToCMYK для [Int] и для RGB в виде функций (аналогично dEqBool из практики)
dToCMYKfromRGB :: DToCMYK RGB
dToCMYKfromRGB = DMkCMYK toCMYKfromRGB
  where
    toCMYKfromRGB :: RGB -> Maybe CMYK
    toCMYKfromRGB x =
      let r' = (fromIntegral (red x) / 255.0) :: Double
          g' = (fromIntegral (green x) / 255.0) :: Double
          b' = (fromIntegral (blue x) / 255.0) :: Double
          k' = 1 - maximum [r', g', b']
          c = if k == 1 then 0 else (round $ (1.0 - r' - k') / (1.0 - k') * 100.0) :: Int
          m = if k == 1 then 0 else (round $ (1.0 - g' - k') / (1.0 - k') * 100.0) :: Int
          y = if k == 1 then 0 else (round $ (1.0 - b' - k') / (1.0 - k') * 100.0) :: Int
          k = round $ k' * 100
       in mkCMYK c m y k

---------------------------------------

-- 3.c Используйте инстансы (0,25 балла)
--     Приведите пример использования инстансов [Int] и RGB реализованных в 3a и 3b (должно получится 4 примера)

-- В тестах

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

pointful :: (a -> b -> c) -> a -> (d -> b) -> d -> c
pointful a b c d = a b (c d)

-- pointful a b c d = a b $ c d            -- минус скобки
-- pointful a b c d = a b $ c $ d          -- $ опять
-- pointful a b c   = a b $ c              -- eta-reduction
-- pointful a b c   = (a b) $ c            -- частичное применение a
-- pointful a b c   = (a b) . c            -- композиция
-- pointful a b c   = (.) (a b) c          -- . is a usual function
-- pointful a b c   = ((.) (a b)) $ c      -- частичное применение (.)
-- pointful a b     = (.) (a b)            -- eta-reduction
-- pointful a b     = (.) $ a b            -- частичное применение (.)
-- pointful a b     = (.) . a $ b          -- композиция
-- pointful a       = (.) . a              -- eta-reduction
-- pointful a       = (.) (.) a            -- . is a usual function
-- pointful a       = ((.) .) $ a          -- частичное применение (.)
-- pointless        = ((.) .)              -- eta-reduction

pointless :: (a -> b -> c) -> a -> (d -> b) -> d -> c
pointless = ((.) .)

------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq)

instance Enum Day where
  toEnum n = case mod n 7 of
    0 -> Monday
    1 -> Tuesday
    2 -> Wednesday
    3 -> Thursday
    4 -> Friday
    5 -> Saturday
    _ -> Sunday

  fromEnum day = case day of
    Monday -> 0
    Tuesday -> 1
    Wednesday -> 2
    Thursday -> 3
    Friday -> 4
    Saturday -> 5
    Sunday -> 6

  succ day = toEnum $ fromEnum day + 1

  pred day = toEnum $ fromEnum day - 1

---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
nextDay :: Day -> Day
nextDay = succ

-- | Возвращает предыдущий день
dayBefore :: Day -> Day
dayBefore = pred

-- | Возвращает количество от текущего до ближайшей субботы
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend day
  | fromEnum day <= 5 = 5 - fromEnum day -- если сегодня суббота то сегодня ближ суббота
  | otherwise = 6

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

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node v []) = Node (f v) []
  fmap f (Node v cs) = Node (f v) (fmap (fmap f) cs)

---------------------------------------

-- 6.c(*) Реализуйте инстанс Functor для пары (1 балл)

data Pair a b = Pair a b
  deriving (Show, Eq)

instance Functor (Pair a) where
  fmap :: (a2 -> b) -> Pair a1 a2 -> Pair a1 b
  fmap f (Pair x y) = Pair x (f y)

-- С какими трудностями вы столкнулись?

-- Там в примерах по ссылке было решение).
-- Видимо это для того, чтобы выполнялись условия
--
-- Identity
--   fmap id == id
-- Composition
--   fmap (f . g) == fmap f . fmap g
--
-- для tuple. Например: (a, b, c) = ((a, b), c)
--
-- fmap (f . g) ((a, b), c) = (a, b, f (g c))
--
-- fmap f . fmap g $ ((a, b), c) =
--      = fmap f ((a, b), g c) =
--         = ((a, b), f (g c))
--
------------------------------------------------------------------------------------------------

-- 7. Класс типов `Bifunctor` (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналогична функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Bifunctor для Either и пары

data Either' a b = Left' a | Right' b
  deriving (Show, Eq)

instance Bifunctor Either' where
  bimap :: (a -> b) -> (c -> d) -> Either' a c -> Either' b d
  bimap f _ (Left' x) = Left' (f x)
  bimap _ g (Right' y) = Right' (g y)

instance Bifunctor Pair where
  bimap :: (a -> b) -> (c -> d) -> Pair a c -> Pair b d
  bimap f g (Pair x y) = Pair (f x) (g y)

------------------------------------------------------------------------------------------------

-- 8. Задайте свой класс типов, опираясь на интересы, хобби и фазу Луны (0,25 балла)

------------------------------------------------------------------------------------------------

data Moba = Dota | Lol | Hots
  deriving (Show)

class MobaPlayer a where
  play :: a -> String

instance MobaPlayer Moba where
    play :: Moba -> String
    play _ = "Loser"
