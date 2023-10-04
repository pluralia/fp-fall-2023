-- 1. Реализуйте логическое И как функцию и как оператор (выберете произвольные символы для вашего оператора).

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

(<^>) :: Bool -> Bool -> Bool
(<^>) x y = myAnd x y

-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Int -> Int
fib n | n < 0     = error "Lower than 0"
      | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib (n - 1) + fib (n - 2)

-- | c хвостовой рекурсией
--
fibTail :: Int -> Either String Int
fibTail n | n < 0     = Left "Negative n"
          | otherwise = Right (helper 0 1 n)
  where
    helper a b n | n == 0    = a
                 | otherwise = helper b (a + b) (n - 1)

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?

-- | без хвостовой рекурсии: Не выполнимо, тк образуется огромное дерево рекурсии и компьютеру становится плохо.
-- | c хвостовой рекурсией: Int переполняется. Integer не переполняется

-------------------------------------------------------------------------------

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--   Подсказка: `((a, b) -> c) -> a -> b -> c` эквивалентно `((a, b) -> c) -> (a -> b -> c)`
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

-- | превращает каррированную функцию в некаррированную
--   Подсказка: `(a -> b -> c) -> (a, b) -> c` эквивалентно `(a -> b -> c) -> ((a, b) -> c)`
--
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (x, y) = f x y

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- | возвращает длину списка
--
myLength :: [a] -> Integer
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- | возвращает хвост списка
--
myTail :: [a] -> Either String [a]
myTail [] = Left "Empty List"
myTail (_ : xs) = Right xs

-- | возвращает список без последнего элемента
--
myInit :: [a] -> Maybe [a]
myInit [] = Nothing
myInit [x] = Just []
myInit (x : xs) = mapMaybe (x :) (myInit xs)

-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend x [] = x
myAppend x (y : ys) = myAppend (x ++ [y]) ys

-- | разворачивает список
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = helper xs [x]
  where helper [] acc       = acc
        helper (x : xs) acc = helper xs (x : acc)

-- | выдаёт элемент списка по индексу
elemByIndex :: [a] -> Integer -> Either String a
elemByIndex [] _ = Left "Index out of range"
elemByIndex (x : xs) ind | myLength (x : xs) <= ind   = Left "Index out of range"
                         | ind < 0                    = Left "Index out of range"
                         | ind == 0                   = Right x
                         | otherwise                  = elemByIndex xs (ind - 1)

-- | Тесты
testMyLength :: Bool
testMyLength = myLength [1, 2, 3, 4, 5] == 5
testMyTail :: Bool
testMyTail = myTail [1, 2, 3, 4, 5] == Right [2, 3, 4, 5]
testMyInit :: Bool
testMyInit = myInit [1, 2, 3, 4, 5] == Just [1, 2, 3, 4]
testMyAppend :: Bool
testMyAppend = myAppend [1, 2, 3, 4, 5] [6, 7, 8, 9, 10] == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
testMyReverse :: Bool
testMyReverse = myReverse [1, 2, 3, 4, 5] == [5, 4, 3, 2, 1]
testElemByIndex :: Bool
testElemByIndex = elemByIndex [1, 2, 3, 4, 5] 2 == Right 3

allTests4 :: Bool
allTests4 = and [testMyLength, testMyTail, testMyInit, testMyAppend, testMyReverse, testElemByIndex]

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
mapList f (x : xs) = (f x : mapList f xs)

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
chSucc x = Succ x 

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd x Zero = x
chAdd x (Succ y) = chAdd (Succ x) y

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult _ Zero = Zero
chMult x (Succ Zero) = x
chMult x (Succ y) = chAdd x (chMult x y)

chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow _ Zero = Succ Zero
chPow x (Succ Zero) = x
chPow x (Succ y) = chMult x (chPow x y)

chPrev :: ChurchNumber -> ChurchNumber
chPrev (Succ x) = x
chPrev Zero = Zero

-- | Тесты

churchOne :: ChurchNumber
churchOne = Succ Zero

churchTwo :: ChurchNumber
churchTwo = Succ (Succ Zero)

churchSix :: ChurchNumber
churchSix = Succ (Succ (Succ churchThree))

churchEight :: ChurchNumber
churchEight = Succ (Succ churchSix)

testChSucc :: Bool
testChSucc = chSucc churchOne == churchTwo
testChAdd :: Bool
testChAdd = chAdd churchOne churchTwo == churchThree
testChMult1 :: Bool
testChMult1 = chMult churchOne churchTwo == churchTwo
testChMult2 :: Bool
testChMult2 = chMult churchTwo churchThree == churchSix
testChPow1 :: Bool
testChPow1 = chPow churchOne churchTwo == churchOne
testChPow2 :: Bool
testChPow2 = chPow churchTwo churchThree == churchEight
testChPrev :: Bool
testChPrev = chPrev churchThree == churchTwo

allTests6 :: Bool
allTests6 = and [testChSucc, testChAdd, testChMult1, testChMult2, testChPow1, testChPow2, testChPrev]

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).

data Point = Point {
    x :: Double,
    y :: Double
}
  deriving (Show, Eq)

--    Реализуйте функции для этого типа.
-- | двигает точку на заданное расстояние по каджой из координат
--
move :: Point -> (Double, Double) -> Point
move (Point x y) (dx, dy) = Point (x + dx) (y + dy)

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2) 

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
binTreeOfInts :: BinaryTree Int
binTreeOfInts = 
  Node 0
    (Node 1 
      (Node 3 Leaf Leaf) 
      (Node 4 Leaf Leaf)
    ) 
    (Node 2
      (Node 5 Leaf Leaf) 
      (Node 6 Leaf Leaf)
    )

type IntTree = BinaryTree Int

binTreeOfIntsCopy :: IntTree
binTreeOfIntsCopy = binTreeOfInts

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--

isPresented :: Int -> IntTree -> Bool
isPresented val Leaf = False
isPresented val (Node nodeValue leftChild rightChild) | val == nodeValue   = True
                                                      | otherwise          = isPresented val leftChild || isPresented val rightChild

-- | Тесты

testIsPresented1 :: Bool
testIsPresented1 = isPresented 3 binTreeOfInts
testIsPresented2 :: Bool
testIsPresented2 = isPresented 10 binTreeOfInts == False

allTests8 :: Bool
allTests8 = and [testIsPresented1, testIsPresented2]

-------------------------------------------------------------------------------

allTests :: Bool
allTests = and [allTests4, allTests6, allTests8]