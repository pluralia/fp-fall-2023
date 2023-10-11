import GHC.Base (VecElem(Int16ElemRep))
-- 1. Реализуйте логическое И как функцию и как оператор (выберете произвольные символы для вашего оператора).

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

infix 3 $$
($$) :: Bool -> Bool -> Bool
True $$ True  = True
_    $$ _     = False
------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | c хвостовой рекурсией
--
fibTail :: Int -> Int
fibTail n = fibHelper 0 1 n
  where
    fibHelper :: Int -> Int -> Int -> Int
    fibHelper a b 0 = b
    fibHelper a b n = fibHelper b (a + b) (n - 1)

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?

-- При вычислении чисел Фибоначчи без хвостовой рекурсии время работы программы возрастает экспоненциально,
-- что можно увидеть при запуске вычислений для числел хотя бы порядка 10^3. 
-- Хвостовая рекурсия же позволяет вычислять нужное число за линию времени.
-- (при запуске на 1500 функция fib бесконечно думает и не выдаёт результат, 
-- а функцияция с хвостовой рекурсией всё хорошо считает - дело в асимптотике).

-------------------------------------------------------------------------------
-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--   Подсказка: `((a, b) -> c) -> a -> b -> c` эквивалентно `((a, b) -> c) -> (a -> b -> c)`
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry func x y = func (x, y)

-- | превращает каррированную функцию в некаррированную
--   Подсказка: `(a -> b -> c) -> (a, b) -> c` эквивалентно `(a -> b -> c) -> ((a, b) -> c)`
--
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry func (x, y) = func x y

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- (head : tail)

-- | возвращает длину списка
--
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + (myLength xs)

-- с хвостовой рекурсией
myLengthTail :: [a] -> Int
myLengthTail array = myLengthHelper array 0
  where
    myLengthHelper :: [a] -> Int -> Int
    myLengthHelper []       len = len
    myLengthHelper (_ : xs) len = myLengthHelper xs (len + 1)

-- | возвращает хвост списка
--
myTail :: [a] -> Either String [a]
myTail [] = Left "The array is empty!"
myTail (x : xs) = Right xs

-- | возвращает список без последнего элемента
--
myInit :: [a] -> Maybe [a]
myInit [] = Nothing
myInit [_] = Just []
myInit (x : xs) = mapMaybe (\t -> x : t) (myInit xs)

-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x : xs) y = x : (myAppend xs y)

-- | разворачивает список
--


myReverse :: [a] -> [a]
myReverse x = myReverseHelper x []
  where
    myReverseHelper :: [a] -> [a] -> [a]
    myReverseHelper [] y = y
    myReverseHelper (x : xs) y = myReverseHelper xs (x : y)

-- | выдаёт элемент списка по индексу (0-индексация)
elemByIndex :: [a] -> Int -> Maybe a
elemByIndex [] _ = Nothing
elemByIndex (x : xs) 0 = Just x
elemByIndex (x : xs) left = elemByIndex xs (left - 1)
-------------------------------------------------------------------------------
-- | Тесты

a :: [Int]
a = [1, 2, 3, 4, 5]
b :: [Int]
b = [6, 7, 8]
c :: [Int]
c = [9]
d :: [Int]
d = []

testLength1 :: Bool
testLength1 = myLength a == length a
testLength2 :: Bool
testLength2 = myLength c == length c
testLength3 :: Bool
testLength3 = myLength d == length d

testTail1 :: Bool
testTail1 = myTail a == Right (tail a)
testTail2 :: Bool
testTail2 = myTail b == Right (tail b)
testTail3 :: Bool
testTail3 = myTail c == Right (tail c)
testTail4 :: Bool
testTail4 = myTail d == Left "The array is empty!"

testInit1 :: Bool
testInit1 = myInit a == Just (init a)
testInit2 :: Bool
testInit2 = myInit b == Just (init b)
testInit3 :: Bool
testInit3 = myInit c == Just (init c)
testInit4 :: Bool
testInit4 = myInit d == Nothing

testAppend1 :: Bool
testAppend1 = myAppend a b == a ++ b
testAppend2 :: Bool
testAppend2 = myAppend c d == c ++ d
testAppend3 :: Bool
testAppend3 = myAppend d a == d ++ a
testAppend4 :: Bool
testAppend4 = myAppend d d == d ++ d

testReverse1 :: Bool
testReverse1 = myReverse a == reverse a
testReverse2 :: Bool
testReverse2 = myReverse b == reverse b
testReverse3 :: Bool
testReverse3 = myReverse c == reverse c
testReverse4 :: Bool
testReverse4 = myReverse d == reverse d

testIndex1 :: Bool
testIndex1 = elemByIndex a 1 == Just (a !! 1)
testIndex2 :: Bool
testIndex2 = elemByIndex a (length a) == Nothing

-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapEither f _ (Left x) = Left (f x)
mapEither _ g (Right x) = Right (g x)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f (Just a) = Just (f a)
mapMaybe _ Nothing = Nothing

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x : xs) = (f x) : (mapList f xs)

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
chSucc ch = Succ ch

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd a Zero = a
chAdd a (Succ b) = chAdd (Succ a) b -- шиворот-навыворотная логика

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult _ Zero = Zero
chMult Zero _ = Zero
chMult a (Succ Zero) = a
chMult a (Succ b) = chAdd a (chMult a b)

chPow :: ChurchNumber -> ChurchNumber -> Maybe ChurchNumber
chPow Zero Zero = Nothing
chPow _ Zero = Just (Succ Zero)
chPow Zero _ = Just Zero
chPow a (Succ b) = mapMaybe (chMult a) maybePrevPow
  where
    maybePrevPow :: Maybe ChurchNumber
    maybePrevPow = chPow a b

chPrev :: ChurchNumber -> Maybe ChurchNumber
chPrev Zero = Nothing
chPrev (Succ x) = Just x

-------------------------------------------------------------------------------
-- | Тесты

churchTwo :: ChurchNumber
churchTwo = Succ (Succ Zero)
churchFive :: ChurchNumber
churchFive = Succ (Succ (Succ (Succ (Succ Zero))))
churchEight :: ChurchNumber
churchEight = Succ (Succ (Succ churchFive))

testChSucc1 :: Bool
testChSucc1 = chSucc churchTwo == churchThree
testChSucc2 :: Bool
testChSucc2 = chSucc Zero == Succ Zero

testChAdd1 :: Bool
testChAdd1 = chAdd churchTwo churchThree == churchFive
testChAdd2 :: Bool
testChAdd2 = chAdd Zero churchThree == churchThree
testChAdd3 :: Bool
testChAdd3 = chAdd Zero Zero == Zero

testChMult1 :: Bool
testChMult1 = chMult Zero churchThree == Zero
testChMult2 :: Bool
testChMult2 = chMult (Succ Zero) churchThree == churchThree
testChMult3 :: Bool
testChMult3 = chMult churchTwo churchThree == Succ churchFive
testChMult4 :: Bool
testChMult4 = chMult Zero Zero == Zero

testChPow1 :: Bool
testChPow1 = chPow Zero Zero == Nothing
testChPow2 :: Bool
testChPow2 = chPow churchFive Zero == Just (Succ Zero)
testChPow3 :: Bool
testChPow3 = chPow Zero churchFive == Just Zero
testChPow4 :: Bool
testChPow4 = chPow churchTwo churchThree == Just churchEight

testChPrev1 :: Bool
testChPrev1 = chPrev (Succ Zero) == Just Zero
testChPrev2 :: Bool
testChPrev2 = chPrev churchThree == Just churchTwo
testChPrev3 :: Bool
testChPrev3 = chPrev Zero == Nothing

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

-- | двигает точку на заданное расстояние по каждой из координат
--
data Point = Point {
  x :: Float,
  y :: Float
}
  deriving (Show)

move :: Point -> Float -> Point
move (Point x y) delta = Point (x + delta) (y + delta)

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Float
dist (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

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
binTreeOfInts =
  Node 5
    (Node 3
      (Node 1 Leaf Leaf)
      (Node 2 Leaf Leaf)
    )
    (Node 7
      (Node 6 Leaf Leaf) Leaf
    )

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--
isPresented :: Int -> IntTree -> Bool
isPresented _ Leaf = False
isPresented findVal (Node val left right) =
  if (val == findVal)
    then True
    else (isPresented findVal left) || (isPresented findVal right)

-------------------------------------------------------------------------------
-- | Тесты

testFindElem1 :: Bool
testFindElem1 = isPresented 5 binTreeOfInts == True
testFindElem2 :: Bool
testFindElem2 = isPresented 3 binTreeOfInts == True
testFindElem3 :: Bool
testFindElem3 = isPresented 4 binTreeOfInts == False
testFindElem4 :: Bool
testFindElem4 = isPresented 2 binTreeOfInts == True
testFindElem5 :: Bool
testFindElem5 = isPresented 7 binTreeOfInts == True
testFindElem6 :: Bool
testFindElem6 = isPresented 6 binTreeOfInts == True
testFindElem7 :: Bool
testFindElem7 = isPresented 10 binTreeOfInts == False

-------------------------------------------------------------------------------
