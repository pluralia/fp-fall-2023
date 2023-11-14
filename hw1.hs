-- | Тесты в этой домашке могут выглядеть так:
testFib1 :: Bool
testFib1 = fib 8 == 21

testFib2 :: Bool
testFib2 = fib 5 == fibTail 5

-- | Вызвать из все разом можно так:
testAll :: Bool
testAll =
  and
    [ testFib1,
      testFib2,
      testchAdd1,
      testchAdd2,
      testChPrev,
      testChSucc,
      testchMult1,
      testchMult2,
      testchAddAndMult,
      testchMultAndPow,
      testBinaryIntTree1,
      testBinaryIntTree2,
      testBinaryIntTree3,
      testBinaryIntTree4,
      testBinaryIntTree5
    ]

-------------------------------------------------------------------------------

-- | 1. Реализуйте логическое И как функцию и как оператор (выберите произвольные символы для вашего оператора).
--
-- myAnd :: Bool -> Bool -> Bool
myAnd a b = a /= b

(<=>) :: Bool -> Bool -> Bool
a <=> b = a /= b

-------------------------------------------------------------------------------
-- costraint on types of a and b
-- (Integral a, Integral b) => a -> b

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | c хвостовой рекурсией
-- fibTail :: (Integral x) => x -> x
-- fibTail a
--   | a < 3 = 1
--   | otherwise = go 2 1 1
--   where
--     go z x c
--       | z == a = c
--       | otherwise = go (z + 1) c (x + c)

-- с хвостовой рекурсией честно списано у Артемия
fibTail :: Integer -> Integer
fibTail n
  | n > 1 = helper 0 1 n
  | otherwise = n
  where
    helper n1 n2 n
      | n == 1 = n2
      | n > 0 = helper n2 (n1 + n2) (n - 1)

-- | без хвостовой рекурсии; because final call is not the result; + is needed
fib :: Integer -> Integer
fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

-- перед тем, как выполнить последнюю операцию + должны быть вычислены left right -> exp

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?

-------------------------------------------------------------------------------

-- Int -> 2^64
-- Integer -> arbitrary

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- f :: a -> (b -> c)     -- which can also be written as    f :: a -> b -> c
-- is the curried form of g :: (a, b) -> c

-- | превращает некаррированную функцию в каррированную
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f a b = f (a, b)

-- | превращает каррированную функцию в некаррированную
fstArg :: (a, b) -> a
fstArg (a, _) = a

scdArg :: (a, b) -> b
scdArg (_, b) = b

myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f p = f (fstArg p) (scdArg p)

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- | возвращает длину списка
myLength :: [Int] -> Int
myLength (_ : xs) = 1 + myLength xs
myLength [] = 0

-- | возвращает хвост списка
myTail :: [Int] -> Either String Int
myTail [] = Left "Error"
myTail [x] = Right x
myTail (_ : xs) = myTail xs

-- | возвращает список без последнего элемента
myInit :: [a] -> [a]
myInit [] = []
myInit [a] = []
myInit (x : xs) = x : myInit xs

-- myAppend :: a -> [a] -> [a]
-- myAppend a [] = [a]
-- myAppend a (x:xs) = x : myAppend a xs

-- | объединяет 2 списка
myAppend :: [a] -> [a] -> [a]
myAppend a b = a ++ b

-- | разворачивает список
myReverse :: [a] -> [a]
myReverse (x : xs) = myReverse xs ++ [x]
myReverse [] = []

-- | выдаёт элемент списка по индексу
elemByIndex :: Int -> [Int] -> Maybe Int
elemByIndex idx (x : xs) = if idx == 0 then Just x else elemByIndex (idx - 1) xs
elemByIndex idx [] = Nothing

-- elemByIndex :: Int -> [Int] -> Either String Int
-- elemByIndex idx (x : xs) = if idx == 0 then Right x else elemByIndex (idx - 1) xs
-- elemByIndex idx [] = Left "Index error"

-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f _ (Left x) = Left (f x)
mapEither _ f (Right x) = Right (f x)

-- mapBoth (*2) (*3) (Left 4)
-- mapBoth (*2) (*3) (Right 4)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just a) = Just (f a)
mapMaybe f Nothing = Nothing

mapList :: (a -> b) -> [a] -> [b]
mapList f (x : xs) = f x : mapList f xs

-- mapList f (x: xs) = [f x] ++ mapList f xs
-- mapList f [] = []
-- mapList f xs = map f xs
-- mapList = map

-------------------------------------------------------------------------------

-- 6. Тип 'ChurchNumber' является представлением чисел Чёрча в Haskell.

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show, Eq)

-- | Видно, что, например, число три, закодированное с помощью типа 'ChurchNumber',
--   является трёхкратным применением конструктора данных 'Succ' (который тоже функция) к 'Zero'
churchThree :: ChurchNumber
churchThree = Succ (Succ (Succ Zero))

-- Реализуйте функции `succ`, `add`, `mult`, `pow` и `prev` для типа 'ChurchNumber'
--    - укажите типы функций
--    - напишите тесты

-- Solution

chSucc :: ChurchNumber -> ChurchNumber
chSucc = Succ

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd a (Succ b) = chAdd (Succ a) b
chAdd a churchZero = a

-- chAdd a (Succ b) = Succ (chAdd a b)

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult a (Succ b) = chAdd a (a `chMult` b)
chMult a Zero = Zero

chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow a (Succ b) = chMult a (a `chPow` b)
chPow a Zero = churchOne

chPrev :: ChurchNumber -> ChurchNumber
chPrev Zero = Zero
chPrev (Succ b) = b

-- | Тесты могут выглядеть так
testChSucc :: Bool
testChSucc = chSucc churchTwo == churchThree
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

-- for tests
churchZero :: ChurchNumber
churchZero = Zero

churchOne :: ChurchNumber
churchOne = Succ Zero

churchTwo :: ChurchNumber
churchTwo = Succ (Succ Zero)

-- tests

testchAdd1 :: Bool
testchAdd1 = Zero `chAdd` churchOne == churchOne

testchAdd2 :: Bool
testchAdd2 = (churchTwo `chAdd` churchOne == churchThree) && (churchOne `chAdd` churchTwo == churchThree)

testchMult1 :: Bool
testchMult1 = churchThree `chMult` churchTwo == churchTwo `chMult` churchThree

testchMult2 :: Bool
testchMult2 = churchThree `chMult` churchOne == churchThree

testchAddAndMult :: Bool
testchAddAndMult = churchThree `chAdd` churchThree == churchThree `chMult` churchTwo

testchMultAndPow :: Bool
testchMultAndPow = (churchThree `chMult` churchThree `chMult` churchThree) == (churchThree `chPow` churchThree)

testChPrev :: Bool
testChPrev = chPrev churchThree == churchTwo

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

data Point = Point
  { x :: Double,
    y :: Double
  }
  deriving (Show)

-- | двигает точку на заданное расстояние по каджой из координат
move :: Point -> Double -> Double -> Point
move (Point x y) dx dy = Point (x + dx) (y + dy)

-- dist (Point 1 1) 1 1

-- | возвращает дистанцию между 2 точками
dist :: Point -> Point -> Double
dist (Point a b) (Point x y) = sqrt ((a - x) * (a - x) + (b - y) * (b - y))

-- dist (Point 1 1) (Point 2 2)
-------------------------------------------------------------------------------

-- 8. Бинарное дерево может быть задано следующим образом:

data BinaryTree a
  = Leaf
  | Node
      { nodeValue :: a,
        leftChild :: BinaryTree a,
        rightChild :: BinaryTree a
      }
  deriving (Show, Eq)

-- | Для примера заведём дерево строк.
binTreeOfStrings :: BinaryTree String
binTreeOfStrings =
  Node
    "This"
    ( Node
        "is"
        (Node "Tree" Leaf Leaf)
        (Node "too" Leaf Leaf)
    )
    ( Node
        "and"
        ( Node
            "don't"
            (Node "forget" Leaf Leaf)
            (Node "me!" Leaf Leaf)
        )
        Leaf
    )

-- | В Haskell можно создавать "синонимы типов". То есть некоторые сокращения для уже существующих типов.
--   Например, мы можем сказать, что тип `BinaryTree String`, можно будет называть как `StringTree`.
type StringTree = BinaryTree String

-- | Доказательство того, что `StringTree` и `BinaryTree String` —- один и тот же тип:
--   `binTreeOfStringsCopy` должна вернуть что-то типа 'StringTree', а мы вернули
--   `binTreeOfStrings`, которая имеет тип 'BinaryTree String', и компилятор на это не ругается.
binTreeOfStringsCopy :: StringTree
binTreeOfStringsCopy = binTreeOfStrings

-- | Создайте синоним типов для дерева на числах `IntTree` и дерево `binTreeOfInts`.
--     - раскомментируйте строку с типом, когда объявите `IntTree`
type IntTree = BinaryTree Int

binTreeOfInts :: IntTree
binTreeOfInts =
  Node
    1
    (Node 2 (Node 3 Leaf Leaf) Leaf)
    (Node 4 Leaf Leaf)

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты

-- честно списано у Артемия (сам долго мучался)
isPresented :: Int -> IntTree -> Bool

isPresented _ Leaf = False
isPresented x (Node val left right) = x == val || isPresented x left || isPresented x right

testBinaryIntTree1 :: Bool
testBinaryIntTree1 = isPresented 1 binTreeOfInts

testBinaryIntTree2 :: Bool
testBinaryIntTree2 = isPresented 2 binTreeOfInts

testBinaryIntTree3 :: Bool
testBinaryIntTree3 = isPresented 3 binTreeOfInts

testBinaryIntTree4 :: Bool
testBinaryIntTree4 = isPresented 4 binTreeOfInts

testBinaryIntTree5 :: Bool
testBinaryIntTree5 = not (isPresented (-1) binTreeOfInts)

-------------------------------------------------------------------------------