{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}


module MyLib where
import Data.Ix
import Data.Bifunctor

------------------------------------------------------------------------------------------------

-- 1. Числа Черча и `Ix` (1 балл)
--    Напишите инстансы `Eq`, `Ord`, `Num` и `Ix` для чисел Черча
data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show)

instance Eq ChurchNumber where
    (==) :: ChurchNumber -> ChurchNumber -> Bool
    Zero == Zero = True
    (Succ n) == (Succ m) = n == m
    _ == _ = False

instance Ord ChurchNumber where
    compare :: ChurchNumber -> ChurchNumber -> Ordering
    compare Zero Zero = EQ
    compare Zero (Succ _) = LT
    compare (Succ _) Zero = GT
    compare (Succ n) (Succ m) = compare n m

instance Num ChurchNumber where
    (+) :: ChurchNumber -> ChurchNumber -> ChurchNumber
    Zero + m = m
    (Succ n) + m = Succ (n + m)

    (*) :: ChurchNumber -> ChurchNumber -> ChurchNumber
    Zero * _ = Zero
    (Succ n) * m = m + (n * m)

    abs :: ChurchNumber -> ChurchNumber
    abs n = n

    signum :: ChurchNumber -> ChurchNumber
    signum Zero = Zero
    signum _ = Succ Zero

    fromInteger :: Integer -> ChurchNumber
    fromInteger x | x <= 0    = Zero
                  | otherwise = Succ (fromInteger (x - 1))

    (-) :: ChurchNumber -> ChurchNumber -> ChurchNumber
    Zero - _ = Zero
    n - Zero = n
    (Succ n) - (Succ m) = n - m

instance Ix ChurchNumber where
    range :: (ChurchNumber, ChurchNumber) -> [ChurchNumber]
    range (n1, m1)
      | n1 > m1     = []
      | n1 == m1    = [m1]
      | otherwise = n1 : range (Succ n1, m1)
    --range (n1, m1) = enumFromToChurch n1 m1
    --  where 
    --    enumFromToChurch :: ChurchNumber -> ChurchNumber -> [ChurchNumber]
    --    enumFromToChurch n2 m2
    --      | n2 > m2     = []
    --      | n2 == m2    = [m2]
    --      | otherwise = n2 : enumFromToChurch (Succ n2) m2

    index :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Int
    index (n1, _) i = fromInteger $ churchToInt  (i - n1)
      where
        churchToInt :: ChurchNumber -> Integer
        churchToInt Zero = 0
        churchToInt (Succ n2) = 1 + churchToInt n2

    inRange :: (ChurchNumber, ChurchNumber) -> ChurchNumber -> Bool
    inRange (n, m) i = i >= n && i < m



-- Вы можете найти класс `Ix` по ссылке:
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Ix.html
-- Обратите внимание на необходимость импорта: `import Data.Ix`

------------------------------------------------------------------------------------------------

-- 2. Дерево (2 балла)

data Tree a = Node
  { value :: a
  , children :: [Tree a]
  }

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
    show (In (Node v [])) = show v
    show (In (Node v ts)) = show v ++ " " ++ unwords ( map (show . In) ts)


-- | pre-order обход дерева
--
newtype PreOrder a = Pre (Tree a)

instance Show a => Show (PreOrder a) where
    show :: Show a => PreOrder a -> String
    show (Pre (Node v [])) = show v
    show (Pre (Node v xs)) = unwords $ show v : map (show . Pre) xs

-- | post-order обход дерева
--
newtype PostOrder a = Post (Tree a)

instance Show a => Show (PostOrder a) where
    show :: Show a => PostOrder a -> String
    show (Post (Node v [])) = show v
    show (Post (Node v xs)) = unwords $ map (show . Post) xs ++ [show v]

-- просто inctance Show - иначе в тестах Should Be ругается 
instance Show a => Show (Tree a) where
    show :: Show a => Tree a -> String
    show (Node v children_show) = "Node " ++ show v ++ " [" ++ unwords (map show children_show) ++ "]"


---------------------------------------

-- 2.b Напишите инстанс `Eq` для дерева (0,5 балла)
instance Eq a => Eq (Tree a) where
    (==) :: Eq a => Tree a -> Tree a -> Bool
    (Node v1 children1) == (Node v2 children2) = v1 == v2 && children1 == children2

------------------------------------------------------------------------------------------------

-- 3. Цвета (2 балла)

-- | Зададим тип данных RGB, но так как его поля должны быть от 0 до 255, назовем конструктор Unsafe...
--
data RGB = UnsafeMkRGB
  { red'   :: Int
  , green' :: Int
  , blue'  :: Int
  } deriving (Show)

-- | ...и зададим новый конструктор, который будет проверять значения полей при инициализации
--
mkRGB :: Int -> Int -> Int -> Maybe RGB
mkRGB red green blue
  | inRange (0, 255) `all` [red, green, blue] = Just $ UnsafeMkRGB red green blue
  | otherwise                                 = Nothing 

-- | Аналогично поступим, задавая тип данных CMYK
--
data CMYK = UnsafeMkCMYK
  { cyan'    :: Int
  , magenta' :: Int
  , yellow'  :: Int
  , black'   :: Int
  } deriving (Show, Eq)

mkCMYK :: Int -> Int -> Int -> Int -> Maybe CMYK
mkCMYK cyan magenta yellow black
  | inRange (0, 100) `all` [cyan, magenta, yellow, black] = Just $ UnsafeMkCMYK cyan magenta yellow black
  | otherwise                                             = Nothing 

---------------------------------------

-- 3.a Напишите инстансы класса ToCMYK для [Int] и для RGB (0,75 балла)

class ToCMYK a where
    toCMYK :: a -> Maybe CMYK

instance ToCMYK [Int] where
    toCMYK :: [Int] -> Maybe CMYK
    toCMYK [c, m, y, k] = mkCMYK c m y k
    toCMYK _ = Nothing


instance ToCMYK RGB where
    toCMYK :: RGB -> Maybe CMYK
    toCMYK rgb = mkCMYK (f r') (f g') (f b') (round $ k * 100)
        where
            UnsafeMkRGB r g b = rgb
            r' = fromIntegral r / 255 :: Double
            g' = fromIntegral g / 255 :: Double
            b' = fromIntegral b / 255 :: Double
            k = 1 - maximum [r', g', b']
            f x = round $ (1 - x - k) / (1 - k) * 100
---------------------------------------

-- 3.b Классы типов -- это функции (1 балл)

-- | задайте класс типов ToCMYK используя data (аналогично Eq' из практики)
--
data DToCMYK a = MkToCMYK { toCMYK' :: a -> Maybe CMYK }


-- | реализуйте класс типов DToCMYK для [Int] и для RGB в виде функций (аналогично dEqBool из практики)
dToCMYKList :: DToCMYK [Int]
dToCMYKList = MkToCMYK toCMYKList
  where
    toCMYKList :: [Int] -> Maybe CMYK
    toCMYKList [c, m, y, k] = mkCMYK c m y k
    toCMYKList _ = Nothing

dToCMYKRGB :: DToCMYK RGB
dToCMYKRGB = MkToCMYK toCMYKRGB
  where
    toCMYKRGB :: RGB -> Maybe CMYK
    toCMYKRGB (UnsafeMkRGB r g b)
      | inRange (0, 255) `all` [r, g, b] = mkCMYK (f r') (f g') (f b') (round $ k * 100)
      | otherwise = Nothing
      where
        r' = fromIntegral r / 255 :: Double
        g' = fromIntegral g / 255 :: Double
        b' = fromIntegral b / 255 :: Double
        k = 1 - maximum [r', g', b']
        f x = round $ (1 - x - k) / (1 - k) * 100
---------------------------------------

-- 3.c Используйте инстансы (0,25 балла)
--     Приведите пример использования инстансов [Int] и RGB реализованных в 3a и 3b (должно получится 4 примера)

-- написано в тестах


------------------------------------------------------------------------------------------------

-- 4. Сделайте функцию `pointful` бесточечной, объясняя каждый шаг по примеру из практики
--    (1,5 балла)

--pointful a b c d = a b (c d)
--                        = (a b) $ c $ d - закидываем c и d в конец
--                        = (a . b) $ c $ d - заменяем (a b) на a.b
--                        = (.)$(.) $ c $ d - заменяем (a . b) на просто композиции в нужном порядке
--                        = (.)$(.) - откидываем c и d так как они последние агрументы


------------------------------------------------------------------------------------------------

-- 5. Дни недели и `Enum` (1 балл)

---------------------------------------

-- 5.a Задайте тип данных "дни недели", в котором перечислите дни недели
--     и реализуйте для него класс типов `Enum`
--     https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Enum

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Show, Eq, Ord, Bounded)

instance Enum Day where
    toEnum :: Int -> Day
    toEnum x = case x `mod` 7 of
        0 -> Monday
        1 -> Tuesday
        2 -> Wednesday
        3 -> Thursday
        4 -> Friday
        5 -> Saturday
        6 -> Sunday
        _ -> error "Error - there are only 7 days in a week!"
    
    fromEnum :: Day -> Int
    fromEnum x = case x of
        Monday -> 0
        Tuesday -> 1
        Wednesday -> 2
        Thursday -> 3
        Friday -> 4
        Saturday -> 5
        Sunday -> 6

    succ :: Day -> Day
    succ x = toEnum $ fromEnum x + 1

    pred :: Day -> Day
    pred x = toEnum $ fromEnum x - 1
  
---------------------------------------

-- 5.b Реализуйте следующие функции

-- | Возвращает следующий день
--
nextDay :: Day -> Day
nextDay = succ

-- | Возвращает предыдущий день
--
dayBefore :: Day -> Day
dayBefore = pred

-- | Возвращает количество от текущего до ближайшей субботы
--
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
  deriving (Show)

instance Functor List where
  fmap :: (a -> b) -> List a -> List b
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

---------------------------------------

-- 6.b Реализуйте инстанс Functor для дерева из задания 2 (0,5 балла)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Node v children_fmap) = Node (f v) (map (fmap f) children_fmap)

---------------------------------------

-- 6.c(*) Реализуйте инстанс Functor для пары (1 балл)

data Pair a b = Pair a b
  deriving (Show)

instance Functor (Pair a) where
  fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair x y) = Pair x (f y)
-- С какими трудностями вы столкнулись? - Pair принимает 2 аргумента, а Functor 1, но что с этим делать я не придумала

------------------------------------------------------------------------------------------------

-- 7. Класс типов `Bifunctor` (0,5 балла)

-- Вы реализовывали функцию mapEither -- она аналочна функции bimap из Bifunctor
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Bifunctor.html#t:Bifunctor

-- Реализуйте инстанс Functor для Either и пары

data MyEither a b = MyLeft a | MyRight b

instance Functor (MyEither a) where
  fmap :: (b -> c) -> MyEither a b -> MyEither a c
  fmap _ (MyLeft x) = MyLeft x
  fmap f (MyRight y) = MyRight (f y)

instance Bifunctor MyEither where
  bimap :: (a -> d) -> (b -> e) -> MyEither a b -> MyEither d e
  bimap f _ (MyLeft x) = MyLeft (f x)
  bimap _ g (MyRight y) = MyRight (g y)

data MyPair a b = MyPair a b

instance Bifunctor MyPair where
  bimap :: (a -> d) -> (b -> e) -> MyPair a b -> MyPair d e
  bimap f g (MyPair x y) = MyPair (f x) (g y)
 
------------------------------------------------------------------------------------------------

-- 8. Задайте свой класс типов, опираясь на интересы, хобби и фазу Луны (0,25 балла)

 -- гороскоп
data MyHoroscope = MyHoroscope { mySign :: String, myPrediction :: String }

class HoroscopeCompatibility h where
  compatible :: h -> h -> Bool
  compatibilityMessage :: h -> h -> String
  compatibilityMessage h1 h2 = if compatible h1 h2
    then "These horoscopes are compatible."
    else "These horoscopes are not compatible."

instance HoroscopeCompatibility MyHoroscope where
  compatible :: MyHoroscope -> MyHoroscope -> Bool
  compatible h1 h2 = mySign h1 == mySign h2

  compatibilityMessage :: MyHoroscope -> MyHoroscope -> String
  compatibilityMessage h1 h2 
    | mySign h1 == "Leo" || mySign h1 == "Scorpio" || mySign h2 == "Leo" || mySign h2 == "Scorpio" = "Think again..."
    | compatible h1 h2 = "The signs " ++ mySign h1 ++ " and " ++ mySign h2 ++ " are compatible."
    | otherwise = "The signs " ++ mySign h1 ++ " and " ++ mySign h2 ++ " are not compatible."

  

------------------------------------------------------------------------------------------------

