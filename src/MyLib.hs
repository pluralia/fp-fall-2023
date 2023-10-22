{-# LANGUAGE FlexibleInstances, InstanceSigs #-}
module MyLib where

import           Data.Ix (Ix, inRange, index, range)
import qualified Data.List as L (intercalate)
-- import           Data.Bifunctor 

------------------------------------------------------------------------------------------------

-- 1. Числа Черча и `Ix` (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча
data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

instance Eq ChurchNumber where
    (==) :: ChurchNumber -> ChurchNumber -> Bool
    Zero     == Zero     = True
    Zero     == _        = False
    _        == Zero     = False
    (Succ n) == (Succ m) = n == m

instance Ord ChurchNumber where
    (<=) :: ChurchNumber -> ChurchNumber -> Bool
    Zero     <= _        = True
    _        <= Zero     = False
    (Succ n) <= (Succ m) = n <= m

instance Num ChurchNumber where
    (+), (*), (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
    abs, signum   :: ChurchNumber -> ChurchNumber
    fromInteger   :: Integer      -> ChurchNumber

    a + Zero     = a
    a + (Succ b) = Succ a + b

    _    * Zero        = Zero
    Zero * _           = Zero
    a    * (Succ Zero) = a
    a    * (Succ b)    = a + (a * b)

    a        - Zero     = a
    Zero     - _        = error "ChurchNumber can't be negative!"
    (Succ n) - (Succ m) = n - m

    abs n = n

    signum Zero = Zero
    signum _    = Succ Zero

    -- для теста (fromInteger 15) :: ChurchNumber
    fromInteger = counter Zero
        where 
            counter :: ChurchNumber -> Integer -> ChurchNumber
            counter n left = if left == 0 then n else counter (Succ n) (left - 1)
    
instance Ix ChurchNumber where
    range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
    index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
    inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool

    range = helperRange []
        where
            helperRange :: [ChurchNumber] -> (ChurchNumber, ChurchNumber) -> [ChurchNumber]
            helperRange arr (l, r) | l >  r    = error "Wrong bound!"
                                   | l == r    = r : arr
                                   | otherwise = helperRange (r : arr) (l, r - Succ Zero)
    
    index = helperInd 0
        where
            helperInd :: Int -> (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
            helperInd counter (l, r) element 
              | l > r                      = error "Wrong bound!"
              | element > r || element < l = error "Element not in bound!"
              | element == l               = counter
              | otherwise                  = helperInd (counter + 1) (l + Succ Zero, r) element
    
    inRange (l, r) element | l > r                = error "Wrong bound!"
                           | otherwise            = l <= element && element <= r

-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

------------------------------------------------------------------------------------------------

-- 2. Дерево (2 балла)

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  }
  deriving (Show)


---------------------------------------

-- 2.a Напишите 3 инстанса `Show` для дерева.
--     Истансы должны реализовывать in-, pre-, post-order обход дерева (1,5 балла)

-- Мы не можем определять несколько инстансов одного типа для одного класса,
-- поэтому обернем наш тип `Tree` в newtype, чтобы у нас было 3 разных типа,
-- и уже для них реализуем `Show`

-- | in-order обход дерева
--
newtype InOrder a = In (Tree a)

instance Show a => Show (InOrder a) where
  show :: Show a => InOrder a -> String
  show (In Node {value=val, children=ch}) 
    | null ch        = show val      --  | length ch == 0 = show val
    | length ch == 1 = (show . In) (head ch) ++ " " ++ show val
    | otherwise            = L.intercalate (" "  ++ show val ++ " ") (map (show . In) ch)

-- | pre-order обход дерева
--
newtype PreOrder a = Pre (Tree a)

instance Show a => Show (PreOrder a) where
  show :: Show a => PreOrder a -> String
  show (Pre Node {value=val, children=ch}) = unwords (show val : map (show . Pre) ch)

-- | post-order обход дерева
--
newtype PostOrder a = Post (Tree a)

instance Show a => Show (PostOrder a) where
  show :: Show a => PostOrder a -> String
  show (Post Node {value=val, children=ch}) = unwords (map (show . Post) ch ++ [show val])

---------------------------------------

-- 2.b Напишите инстанс `Eq` для дерева (0,5 балла)

instance Eq a => Eq (Tree a) where
  (==) :: Eq a => Tree a -> Tree a -> Bool
  Node {value=val_a, children=child_a} == Node {value=val_b, children=child_b} = 
    (val_a == val_b) && all (uncurry (==)) (zip child_a child_b) && (length child_a == length child_b)

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
mkRGB red' green' blue'
  | inRange (0, 255) `all` [red', green', blue'] = Just $ UnsafeMkRGB red' green' blue'
  | otherwise                                    = Nothing 

-- | Аналогично поступим, задавая тип данных CMYK
--
data CMYK = UnsafeMkCMYK
  { cyan    :: Int
  , magenta :: Int
  , yellow  :: Int
  , black   :: Int
  } deriving (Show, Eq)

mkCMYK :: Int -> Int -> Int -> Int -> Maybe CMYK
mkCMYK cyan' magenta' yellow' black'
  | inRange (0, 100) `all` [cyan', magenta', yellow', black'] = Just $ UnsafeMkCMYK cyan' magenta' yellow' black'
  | otherwise                                                 = Nothing 

---------------------------------------

-- 3.a Напишите инстансы класса ToCMYK для [Int] и для RGB (0,75 балла)

class ToCMYK a where
  toCMYK :: a -> Maybe CMYK

instance ToCMYK [Int] where 
  toCMYK :: [Int] -> Maybe CMYK
  toCMYK arr | length arr == 4 = mkCMYK (head arr) (arr !! 1) (arr !! 2) (arr !! 3)
             | otherwise       = Nothing

instance ToCMYK RGB where
  toCMYK :: RGB -> Maybe CMYK
  toCMYK (UnsafeMkRGB {red=r, green=g, blue=b}) = mkCMYK c m y (round (k * 100)) 
    where
      r' = (fromIntegral r :: Double) / 255
      g' = (fromIntegral g :: Double) / 255
      b' = (fromIntegral b :: Double) / 255
      k = 1 - maximum [r', g', b']
      c = if k == 1 then 0 else round (((1 - r' - k) / (1 - k)) * 100)
      m = if k == 1 then 0 else round (((1 - g' - k) / (1 - k)) * 100)
      y = if k == 1 then 0 else round (((1 - b' - k) / (1 - k)) * 100)

-- fromIntegral - для перевода Int to Double

---------------------------------------

-- 3.b Классы типов -- это функции (1 балл)

-- | задайте класс типов ToCMYK используя data (аналогично Eq' из практики)
newtype DToCMYK a = MkCMYK { toCMYK' :: a -> Maybe CMYK }

-- | реализуйте класс типов DToCMYK для [Int] и для RGB в виде функций (аналогично dEqBool из практики)

dMkCMYKfromInt :: DToCMYK [Int]
dMkCMYKfromInt = MkCMYK toCMYK''
  where
    toCMYK'' :: [Int] -> Maybe CMYK
    toCMYK'' arr | length arr == 4 = mkCMYK (head arr) (arr !! 1) (arr !! 2) (arr !! 3)
                | otherwise        = Nothing

dMkCMYKfromRGB :: DToCMYK RGB
dMkCMYKfromRGB = MkCMYK toCMYK''' 
  where
    toCMYK''' :: RGB -> Maybe CMYK
    toCMYK''' (UnsafeMkRGB {red=r, green=g, blue=b}) = mkCMYK c m y (round (k * 100)) 
      where
        r' = (fromIntegral r :: Double) / 255
        g' = (fromIntegral g :: Double) / 255
        b' = (fromIntegral b :: Double) / 255
        k = 1 - maximum [r', g', b']
        c = if k == 1 then 0 else round (((1 - r' - k) / (1 - k)) * 100)
        m = if k == 1 then 0 else round (((1 - g' - k) / (1 - k)) * 100)
        y = if k == 1 then 0 else round (((1 - b' - k) / (1 - k)) * 100)  

---------------------------------------

-- 3.c Используйте инстансы (0,25 балла)
--     Приведите пример использования инстансов [Int] и RGB реализованных в 3a и 3b (должно получится 4 примера)

purpure :: [Int]
purpure = [50, 60, 0, 0]
purpureRGB :: RGB
purpureRGB = UnsafeMkRGB 128 0 128

purpureFromInt :: Maybe CMYK
purpureFromInt = toCMYK purpure
purpureFromRGB :: Maybe CMYK
purpureFromRGB = toCMYK purpureRGB

purpureFromInt' :: Maybe CMYK
purpureFromInt' = toCMYK' dMkCMYKfromInt purpure
purpureFromRGB' :: Maybe CMYK
purpureFromRGB' = toCMYK' dMkCMYKfromRGB purpureRGB
------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

pointful :: (a -> c -> d) -> a -> (b -> c) -> b -> d
-- pointful f x g y = f x (g y)  
-- pointful f x g y = f x $ g y       -- remove braces with $
-- pointful f x g y = ((f x) . g) y   -- use composition .
-- pointful f x g y = (f x) . g $ y   -- remove braces with $
-- pointful f x g   = (f x) . g       -- eta-reduction
-- pointful f x g   = (.) (f x) g     -- . is a usual function
-- pointful f x     = (.) (f x)       -- eta-reduction
-- pointful f x     = (.) $ f x       -- remove braces with $
-- pointful f x     = (.) . f $ x     -- use composition .
-- pointful f       = (.) . f         -- eta-reduction
-- pointful f       = (.) (.) f       -- . is a usual function
pointful            = (.) (.)         -- eta-reduction

------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day =   Sun
           | Mon
           | Tue
           | Wed
           | Thu
           | Fri
           | Sat
  deriving (Show, Eq)

instance Enum Day where
  toEnum :: Int -> Day
  toEnum n | n == 7    = Sun
           | n == 1    = Mon
           | n == 2    = Tue
           | n == 3    = Wed
           | n == 4    = Thu
           | n == 5    = Fri
           | n == 6    = Sat
           | otherwise = error "You must enter number from 1 to 7."

  fromEnum :: Day -> Int
  fromEnum d | d == Sun  = 7
             | d == Mon  = 1
             | d == Tue  = 2
             | d == Wed  = 3
             | d == Thu  = 4
             | d == Fri  = 5
             | d == Sat  = 6
             | otherwise = error "Uncorrect day."

-- пример для тестов: toEnum 6 :: Day - важно указывать тип результата

---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
--
nextDay :: Day -> Day
nextDay d | d == Sun  = Mon 
          | otherwise = toEnum (fromEnum d + 1)

-- | Возвращает предыдущий день
--
dayBefore :: Day -> Day
dayBefore d | d == Mon  = Sun 
            | otherwise = toEnum (fromEnum d - 1)

-- | Возвращает количество от текущего до ближайшей субботы
--
daysBeforeWeekend :: Day -> Int
daysBeforeWeekend d | d == Sun  = 6
                    | otherwise = 6 - fromEnum d

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
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- обычный паттерн-матчинг
---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева из задания 2 (0,5 балла)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b 
  fmap f node = Node {value = f (value node), children = fmap (fmap f) (children node)} 

-- data Tree a = Node
--   { value :: a
--   , children :: [Tree a]
--   }

---------------------------------------

-- 6.c(*) Реализуйте инстанс Functor для пары (1 балл)

data Pair a b = Pair a b
  deriving (Show, Eq)

instance Functor (Pair a) where
  fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair x y) = Pair x (f y)

-- С какими трудностями вы столкнулись?

-- для любого конструктора более чем одним параметром (например, Either) 
-- только последний параметр может быть изменен с помощью fmap (например, b в `Either a b`).

------------------------------------------------------------------------------------------------

-- 7. Класс типов `Bifunctor` (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Functor для Either и пары

class MyBifunctor p where
   first'  :: (a -> c) -> p a b -> p c b
   second' :: (b -> c) -> p a b -> p a c


instance MyBifunctor Pair where
  first' :: (a -> c) -> Pair a b -> Pair c b
  first' f (Pair x y) = Pair (f x) y

  second' :: (b -> c) -> Pair a b -> Pair a c
  second' f (Pair x y) = Pair x (f y)


instance MyBifunctor Either where
  first' :: (a -> c) -> Either a b -> Either c b
  first' _ (Right _) = error "for ' first' ' must be used (Left _). If you want (Right _) => used ' second' '"
  first' f (Left x)  = Left (f x)

  second' :: (b -> c) -> Either a b -> Either a c
  second' _ (Left _)  = error "for ' second' ' must be used (Right _). If you want (Left _) => used ' first' '"
  second' f (Right y) = Right (f y)

-- Duplicate instance declarations:
--       instance Bifunctor Either -- Defined at src\MyLib.hs:398:10
--       instance [safe] Bifunctor Either -- Defined in `Data.Bifunctor'

-- как правильно импортировать Bifunctor, чтобы не импортировался встроенный для Either?

------------------------------------------------------------------------------------------------

-- 8. Задайте свой класс типов, опираясь на интересы, хобби и фазу Луны (0,25 балла)

class Smiler a where
  giveSmile :: a -> String

instance Smiler Int where
  giveSmile :: Int -> String
  giveSmile x | x < 0  = "(T_T)"
              | x == 0 = "(-_-)"
              | x < 100 = "(o_o)"
              | otherwise = "(^_^)"

------------------------------------------------------------------------------------------------
