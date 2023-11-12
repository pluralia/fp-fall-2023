import Distribution.FieldGrammar (List)
import Data.Maybe (isNothing)
-- | Тесты в этой домашке могут выглядеть так:
--
testFib1 :: Bool
testFib1 = fib 8 == 21

testFib2 :: Bool
testFib2 = undefined

-- | Вызвать из все разом можно так:
testAll :: Bool
testAll = and [
    testFib1
  , testFib2
  ]

-------------------------------------------------------------------------------

-- | 1. Реализуйте логическое И как функцию и как оператор (выберите произвольные символы для вашего оператора).
--
myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

(&) :: Bool -> Bool -> Bool
(&) True True = True
(&) _ _ = False
infixr 3 &
-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib (-1) = 1
fib (-2) = -1
fib n | n >= 0 = fib(n - 1) + fib(n - 2)
fib n | n < 0 = fib(n + 2) - fib(n + 1)

-- | c хвостовой рекурсией
--
fibTail :: Integer -> Integer
fibTail n | n > 1 = helper 0 1 n
fibTail n | n < 0 = helper 0 1 n
fibTail n = n

helper n1 n2 1 = n2
helper n1 n2 (-1) = n2
helper n1 n2 n | n > 0 = helper n2 (n1 + n2) (n - 1) 
helper n1 n2 n | n < 0 = helper n2 (n1 - n2) (n + 1)

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?
-- fib 1000 я не дождался. fibTail 100000 ждал результат пару секунд. Проблема в том, что при обычной рекурсии в Фибоначчи
-- операция суммы -- последняя, а значит чтобы ее выполнить нужно вычислить значения слагаемых, и в итоге получаем очень много
-- функций на стеке (и помимо этого функции от одинаковых аргументов вычисляются несколько раз) -- число рекурсивных вызовов квадратично.
-- С хвостовой рекурсией получаем линейное число рекурсивных вызовов.

-------------------------------------------------------------------------------

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f a b = f (a, b)

-- | превращает каррированную функцию в некаррированную
--
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (a, b) = f a b

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- | возвращает длину списка
--
myLength :: [a] -> Int
myLength [] = 0
myLength n = lenHelper 0 n

lenHelper :: Int -> [a] -> Int
lenHelper n [] = n
lenHelper n (_:xs) = lenHelper (n + 1) xs

-- | возвращает хвост списка
--
myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_ : xs) = Just xs

-- | возвращает список без последнего элемента
--
myInit :: [a] -> Maybe [a]
myInit [x] = Just []
myInit (x:xs) = Just (x : myInit' xs)
myInit [] = Nothing

myInit' :: [a] -> [a]
myInit' [x] = []
myInit' (x:xs) = x : myInit' xs
-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend [] xs = xs
myAppend xs [] = xs
myAppend (x:xs) ys = x : myAppend xs ys

-- | разворачивает список
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myAppend (myReverse xs) [x]

-- | выдаёт элемент списка по индексу
--
elemByIndex :: [a] -> Int -> Maybe a
elemByIndex [] _ = Nothing
elemByIndex xs n | n < 0 || n >= myLength xs = Nothing
elemByIndex (x:xs) 0 = Just x
elemByIndex (x:xs) n = elemByIndex xs (n - 1)

-- Tests for arrays
-- test MyLength
testNonEmpty :: Bool
testNonEmpty = myLength [1, 2, 3, 4, 5] == 5
testEmpty :: Bool
testEmpty = myLength [] == 0

testMyLength :: Bool
testMyLength = testNonEmpty & testEmpty

-- test MyTail
testTailNonEmpty :: Bool
testTailNonEmpty = myTail [1, 2, 3] == Just [2, 3]
testTailEmpty :: Bool
testTailEmpty = isNothing (myTail [])

testMyTail :: Bool
testMyTail = testTailNonEmpty & testTailEmpty

-- test MyInit
testInitNonEmpty :: Bool
testInitNonEmpty = myInit [1, 2, 3] == Just [1, 2]
testInitEmpty :: Bool
testInitEmpty = isNothing (myTail [])

testMyInit :: Bool
testMyInit = testInitNonEmpty & testInitEmpty

-- test MyAppend
testMyAppend0 :: Bool
testMyAppend0 = myAppend [1, 2, 3] [4, 5] == [1, 2, 3, 4, 5]
testMyAppend1 :: Bool
testMyAppend1 = null (myAppend [] [])
testMyAppend2 :: Bool
testMyAppend2 = myAppend [] [4, 5] == [4, 5]
testMyAppend3 :: Bool
testMyAppend3 = myAppend [4, 5] [] == [4, 5]

testMyAppend :: Bool
testMyAppend = testMyAppend0 & testMyAppend1 & testMyAppend2 & testMyAppend3

-- test MyReverse
testMyReverse0 :: Bool
testMyReverse0 = myReverse [1, 2, 3] == [3, 2, 1]
testMyReverse1 :: Bool
testMyReverse1 = null (myReverse [])
testMyReverse2 :: Bool
testMyReverse2 = myReverse [1] == [1]

testMyReverse :: Bool
testMyReverse = testMyReverse0 & testMyReverse1 & testMyReverse2

-- test ElemByIndex
testElemByIndex0 :: Bool
testElemByIndex0 = elemByIndex [10, 20, 30, 40] 2 == Just 30
testElemByIndex1 :: Bool
testElemByIndex1 = isNothing (elemByIndex [10, 20, 30, 40] (-1))
testElemByIndex2 :: Bool
testElemByIndex2 = isNothing (elemByIndex [10, 20, 30, 40] 5)
testElemByIndex3 :: Bool
testElemByIndex3 = isNothing (elemByIndex [] 0)
testElemByIndex4 :: Bool
testElemByIndex4 = elemByIndex [10, 20, 30, 40] 0 == Just 10

testElemByIndex = testElemByIndex0 & testElemByIndex1 & testElemByIndex2 & testElemByIndex3 & testElemByIndex4

testArrays :: Bool
testArrays = and [
    testMyLength,
    testMyLength,
    testMyInit,
    testMyAppend,
    testMyReverse,
    testElemByIndex
  ]
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither f _ (Left x) = Left (f x)
mapEither _ g (Right y) = Right (g y)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

-------------------------------------------------------------------------------

-- 6. Тип 'ChurchNumber' является представлением чисел Чёрча в Haskell.

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show, Eq)

-- | Видно, что, например, число три, закодированное с помощью типа 'ChurchNumber',
--   является трёхкратным применением конструктора данных 'Succ' (который тоже функция) к 'Zero'
--
churchThree :: ChurchNumber
churchThree = Succ (Succ (Succ Zero))

-- Реализуйте функции `succ`, `add`, `mult`, `pow` и `prev` для типа 'ChurchNumber'
--    - укажите типы функций
--    - напишите тесты

chSucc :: ChurchNumber -> ChurchNumber
chSucc = Succ

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd n Zero = n
chAdd n (Succ m) = chAdd (Succ n) m

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult _ Zero = Zero
chMult n (Succ m) = chAdd n (chMult n m)

chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow _ Zero = Succ Zero
chPow n (Succ m) = chMult n (chPow n m)

chPrev :: ChurchNumber -> ChurchNumber
chPrev Zero = Zero
chPrev (Succ n) = n

-- | Тесты могут выглядеть так
--
testChSucc :: Bool
testChSucc = chSucc churchTwo == churchThree
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

-- Тест для функции chAdd
testChAdd :: Bool
testChAdd = chAdd churchTwo churchThree == churchFive
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchFive :: ChurchNumber
    churchFive = Succ (Succ (Succ (Succ (Succ Zero))))

-- chMult
testChMult :: Bool
testChMult = chMult churchThree churchTwo == churchSix
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchSix :: ChurchNumber
    churchSix = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

-- chPow
testChPow :: Bool
testChPow = chPow churchTwo churchThree == churchEight
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchEight :: ChurchNumber
    churchEight = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

-- chPrev
testChPrev :: Bool
testChPrev = chPrev churchThree == churchTwo
  where
    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

testsChurch :: Bool
testsChurch = testChSucc & testChAdd & testChMult & testChPow & testChPrev

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

data Point = Point { xCoord :: Double, yCoord :: Double } 
  deriving (Show)

-- | двигает точку на заданное расстояние по каджой из координат
--

move :: Point -> Double -> Double -> Point
move (Point x y) dx dy = Point (x + dx) (y + dy)

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt ((x1 - x2) ^ 2 + (y1 - y2) ^ 2)

-------------------------------------------------------------------------------

-- 8. Бинарное дерево может быть задано следующим образом:

data BinaryTree a 
  = Leaf
  | Node 
    { nodeValue  :: a
    , leftChild  :: BinaryTree a
    , rightChild :: BinaryTree a
    }
  deriving (Show, Eq)

-- | Для примера заведём дерево строк.
--
binTreeOfStrings :: BinaryTree String
binTreeOfStrings = 
  Node "This" 
    (Node "is" 
      (Node "Tree" Leaf Leaf) 
      (Node "too" Leaf Leaf)
    ) 
    (Node "and" 
      (Node "don't" 
        (Node "forget" Leaf Leaf) 
        (Node "me!" Leaf Leaf)
      ) 
      Leaf
    )

-- | В Haskell можно создавать "синонимы типов". То есть некоторые сокращения для уже существующих типов.
--   Например, мы можем сказать, что тип `BinaryTree String`, можно будет называть как `StringTree`.
--
type StringTree = BinaryTree String

-- | Доказательство того, что `StringTree` и `BinaryTree String` —- один и тот же тип:
--   `binTreeOfStringsCopy` должна вернуть что-то типа 'StringTree', а мы вернули
--   `binTreeOfStrings`, которая имеет тип 'BinaryTree String', и компилятор на это не ругается.
--
binTreeOfStringsCopy :: StringTree
binTreeOfStringsCopy = binTreeOfStrings

-- | Создайте синоним типов для дерева на числах `IntTree` и дерево `binTreeOfInts`.
--     - раскомментируйте строку с типом, когда объявите `IntTree`
--
type IntTree = BinaryTree Int

binTreeOfInts :: IntTree
binTreeOfInts = Node 5 (Node 3 Leaf Leaf) (Node 8 (Node 7 Leaf Leaf) (Node 10 Leaf Leaf))


-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--
isPresented :: Int -> IntTree -> Bool
isPresented _ Leaf = False
isPresented x (Node val left right) = x == val || isPresented x left || isPresented x right

-------------------------------------------------------------------------------

testIsPresented0 :: Bool
testIsPresented0 = isPresented 7 binTreeOfInts

testIsPresented1 :: Bool
testIsPresented1 = isPresented 8 binTreeOfInts

testIsPresented2 :: Bool
testIsPresented2 = not (isPresented 0 binTreeOfInts)

testIsPresented3 :: Bool
testIsPresented3 = not (isPresented 2 binTreeOfInts)

testIsPresented :: Bool
testIsPresented = testIsPresented0 & testIsPresented1 & testIsPresented2 & testIsPresented3