-- | Тесты в этой домашке могут выглядеть так:
--
testFib1 :: Bool
testFib1 = fib 8 == 21

testFib2 :: Bool
testFib2 = fibTail 100 == 354224848179261915075

testOper1 :: Bool
testOper1 = myAnd True True == True

testLength :: Bool
testLength = myLength [1,2,3] == 3

testTail :: Bool
testTail = myTail [1, 2, 3] == Just [2, 3]

testInit :: Bool
testInit = myInit [1,2,4,5] == Just [1,2,4]

testAppend :: Bool
testAppend = myAppend [1,2,3,4,5] [] == [1,2,3,4,5]

testReverse :: Bool
testReverse = myReverse [1,2,3,4] == [4,3,2,1]

testByIndex :: Bool
testByIndex = elemByIndex [1,2,3,4] 2 == Just 2


-- | Вызвать из все разом можно так:
testAll :: Bool
testAll = and [
    testFib1
  , testFib2
  , testOper1
  , testLength
  , testTail
  , testInit
  , testAppend
  , testReverse
  , testByIndex
  , testChAdd
  , testChMult
  , testChPow
  , testChPrev
  ]

-------------------------------------------------------------------------------

-- | 1. Реализуйте логическое И как функцию и как оператор (выберите произвольные символы для вашего оператора).
--
myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

infixr 3 &&&
(&&&) :: Bool -> Bool -> Bool
(&&&) True True = True
(&&&) _ _ = False

-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- | c хвостовой рекурсией
--
fibTail :: Int -> Integer
fibTail n = go n (0,1)
  where
    go !n (!a, !b) | n==0      = a
                   | otherwise = go (n-1) (b, a+b)
-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?
-- Хвостовая реккурсия значительно быстрее работает при вычислении большого числа фиб.
--  количество элементов накапливается в стек. Хвостовую реккурсию компилятор переделывает в цикл и выдает последний результат обращения к вспом. фун.
-------------------------------------------------------------------------------

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry a b c = a (b, c)

-- | превращает каррированную функцию в некаррированную
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry  a (b, c) = a b c

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- | возвращает длину списка
--
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- | возвращает хвост списка
--
myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_:xs) = Just xs

-- | возвращает список без последнего элемента
--
myInit::[a]-> Maybe [a]
myInit [] = Nothing
myInit [_] = Just []
myInit (x:xs) = case myInit xs of
  Nothing -> Nothing
  Just y -> Just (x : y)
  
-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend [] join = join
myAppend (x:xs) join = x : myAppend xs join

-- | разворачивает список
--
myReverse :: [a] -> [a]
myReverse list = rev list []

rev :: [a] -> [a] -> [a]
rev [] acc = acc
rev (x:xs) acc = rev xs (x:acc)


-- | выдаёт элемент списка по индексу
--
elemByIndex :: [a] -> Int -> Maybe a
elemByIndex [] _ = Nothing
elemByIndex _ 0 = Nothing
elemByIndex (x : _) 1 = Just x
elemByIndex (x : xs) n
  | n < 0 = Nothing
  | otherwise = elemByIndex xs (n - 1)






-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither f _ (Left a) = Left (f a)
mapEither _ g (Right b) = Right (g b)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

-------------------------------------------------------------------------------

-- 6. Тип 'ChurchNumber' является представлением чисел Чёрча в Haskell.

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show, Eq)

chSucc :: ChurchNumber -> ChurchNumber
chSucc n = Succ n

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd m n = case m of
  Zero   -> n
  Succ x -> chSucc (chAdd x n)

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult m n = case m of
  Zero   -> Zero
  Succ x -> chAdd n (chMult x n)

chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow m n = case n of
  Zero   -> chSucc Zero
  Succ x -> chMult m (chPow m x)

chPrev :: ChurchNumber -> ChurchNumber
chPrev n = case n of
  Zero       -> Zero
  Succ x     -> x

-- Тесты
churchOne, churchTwo, churchThree :: ChurchNumber
churchOne = Succ Zero
churchTwo = Succ churchOne
churchThree = Succ churchTwo

testChAdd :: Bool
testChAdd = chAdd churchTwo churchThree == churchFive
  where
    churchFive :: ChurchNumber
    churchFive = Succ (Succ (Succ (Succ (Succ Zero))))

testChMult :: Bool
testChMult = chMult churchTwo churchThree == churchSix
  where
    churchSix :: ChurchNumber
    churchSix = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

testChPow :: Bool
testChPow = chPow churchTwo churchThree == churchSeven
  where
    churchSeven :: ChurchNumber
    churchSeven = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))

testChPrev :: Bool
testChPrev = chPrev churchThree == churchTwo


-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

-- | двигает точку на заданное расстояние по каджой из координат
--
data Point  = Point { x :: Float, y :: Float }
  deriving(Show)

move :: Point -> Float -> Point
move (Point x1 y1) distance = Point (x1 + distance) (y1 + distance)


-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Float
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
-- binTreeOfInts :: IntTree
type IntTree = BinaryTree Int

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
binTreeOfInts :: IntTree
binTreeOfInts = 
  Node 5 
    (Node 3 
      (Node 2 Leaf Leaf) 
      (Node 4 Leaf Leaf)
    ) 
    (Node 8 
      (Node 7 
        (Node 6 Leaf Leaf) 
        (Node 9 Leaf Leaf)
      ) 
      Leaf
    )

isPresented :: Int -> IntTree -> Bool
isPresented _ Leaf = False
isPresented target (Node value left right)
  -- | target == value = True
  | otherwise       = isPresented target right

testIsPresented :: Bool
testIsPresented =
  and
    [ isPresented 6 binTreeOfInts 
    , not (isPresented 10 binTreeOfInts) 
    , isPresented 5 binTreeOfInts 
    , isPresented 2 binTreeOfInts  
    , not (isPresented 1 binTreeOfInts)
    ]
-------------------------------------------------------------------------------