-------------------------------------------------------------------------------

-- | 1. Реализуйте логическое И как функцию и как оператор (выберите произвольные символы для вашего оператора).
--
-- Как функция:
myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

-- Как оператор:
(*&*) :: Bool -> Bool -> Bool
infixr 3 *&*
(*&*) True True = True
(*&*) _ _ = False

-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- | c хвостовой рекурсией
--
fibTail :: Int -> Integer
fibTail n = go 0 1 n
  where
    go a b n  | n == 0 = a
              | otherwise = go b (a + b) (n - 1)
      

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?
-- Без хвостовой рекурсии - бесконечно долго вычисляется, с хвостовой рекурсией - мгновенно.
-- Так происходит, потому что обычная рекурсия накапливает результат и запоминает все вычисления,
-- а хвостовая эффективно преобразуется в цикл и выдает результат последнего обращения к вспомог. функции.

-------------------------------------------------------------------------------

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f (x, y)

-- | превращает каррированную функцию в некаррированную
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
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = helper 1 xs 
  where
    helper acc [] = acc
    helper acc (x : xs) = helper (acc + 1) xs


-- | возвращает хвост списка
--
myTail :: [a] -> Either String [a]
myTail [] = Left "This list is empty"
myTail (x : xs) = Right xs

-- | возвращает список без последнего элемента
--
myInit :: [a] -> [a]
myInit [] = []
myInit [_] = []
myInit (x : xs) = (x : myInit xs)


-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend x [] = x
myAppend [] x = x
myAppend (x : xs) y = (x : myAppend xs y)  

-- | разворачивает список
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x : xs) = helper xs [x]
  where
    helper [] y = y
    helper (x : xs) y = helper xs (x : y)

-- | выдаёт элемент списка по индексу
--
elemByIndex :: [a] -> Int -> Either String a
elemByIndex (x : _) 1 = Right x
elemByIndex (x : xs) idx | (idx > (myLength (x : xs))) || (idx <= 0) = Left "Index error" 
                         | otherwise = elemByIndex xs (idx - 1) 

-- Тесты
--
emptyList :: [Int]
emptyList = []

negIdx :: Int
negIdx = -2

testMyLength :: Bool
testMyLength = myLength [1, 2, 3] == 3

testMyTail1 :: Bool
testMyTail1 = myTail [1, 2, 3, 4] == Right [2, 3, 4]

testMyTail2 :: Bool
testMyTail2 = myTail emptyList == Left "This list is empty"

testMyInit1 :: Bool
testMyInit1 = myInit [1, 2, 3] == [1, 2]

testMyInit2 :: Bool
testMyInit2 = myInit [1] == []

testMyInit3 :: Bool
testMyInit3 = myInit emptyList == emptyList

testMyAppend1 :: Bool
testMyAppend1 = myAppend [1, 2, 3] [4, 5, 6] == [1, 2, 3, 4, 5, 6]

testMyAppend2 :: Bool
testMyAppend2 = myAppend [] [4, 5, 6] == [4, 5, 6]

testMyReverse :: Bool
testMyReverse = myReverse [1, 2, 3] == [3, 2, 1]

testElemByIndex1 :: Bool
testElemByIndex1 = elemByIndex [1, 2, 3] 2 == Right 2

testElemByIndex2 :: Bool
testElemByIndex2 = elemByIndex [1, 2, 3] negIdx == Left "Index error"

testElemByIndex3 :: Bool
testElemByIndex3 = elemByIndex emptyList 3 == Left "Index error"

-- Не очень понимаю, почему так происходит:
-- Изначально я задала функцию testElemByIndex2 как 'elemByIndex [1, 2, 3] -2 == Left "Index error"'
-- и получала ошибку 'Couldn't match expected type: Int -> Either String a8 with actual type: Either String b0.'
-- Как только добавила отдельную переменную negIdx все заработало...

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
mapList f (x : xs) = (f x) : (mapList f xs)

-------------------------------------------------------------------------------

-- 6. Тип 'ChurchNumber' является представлением чисел Чёрча в Haskell.

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show, Eq)

-- | Видно, что, например, число три, закодированное с помощью типа 'ChurchNumber',
--   является трёхкратным применением конструктора данных 'Succ' (который тоже функция) к 'Zero'
--
-- churchThree :: ChurchNumber
-- churchThree = Succ (Succ (Succ Zero))

-- Реализуйте функции `succ`, `add`, `mult`, `pow` и `prev` для типа 'ChurchNumber'
--    - укажите типы функций
--    - напишите тесты

chSucc :: ChurchNumber -> ChurchNumber
chSucc x = Succ x

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd x Zero = x
chAdd x (Succ y) = Succ (chAdd x y)


chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult _ Zero = Zero
chMult x (Succ Zero) = x
chMult x (Succ y) = chAdd x (chMult x y) 


chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow _ Zero = (Succ Zero)
chPow x (Succ Zero) = x
chPow x (Succ y) = chMult x (chPow x y)

chPrev :: ChurchNumber -> ChurchNumber
chPrev Zero = Zero
chPrev (Succ x) = x

-- Закодируем числа с помощью типа 'ChurchNumber' для тестов
--
churchZero :: ChurchNumber
churchZero = Zero

churchOne :: ChurchNumber
churchOne = Succ Zero

churchTwo :: ChurchNumber
churchTwo = Succ (Succ Zero)

churchThree :: ChurchNumber
churchThree = Succ (Succ (Succ Zero))

churchFour :: ChurchNumber
churchFour = Succ (Succ (Succ (Succ Zero)))

churchFive :: ChurchNumber
churchFive = Succ (Succ (Succ (Succ (Succ Zero))))

-- Тесты для функций
--
testChSucc :: Bool
testChSucc = chSucc churchTwo == churchThree

testChAdd1 :: Bool
testChAdd1 = chAdd churchOne churchThree == churchFour

testChAdd2 :: Bool
testChAdd2 = chAdd churchZero churchThree == churchThree

testChMult1 :: Bool
testChMult1 = chMult churchZero churchTwo == Zero

testChMult2 :: Bool
testChMult2 = chMult churchOne churchThree == churchThree

testChPow1 :: Bool
testChPow1 = chPow churchFive Zero == churchOne

testChPow2 :: Bool
testChPow2 = chPow churchTwo churchTwo == churchFour

testChPrev :: Bool
testChPrev = chPrev churchFive == churchFour

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

data Point = Point {
  x :: Double, 
  y :: Double
}
  deriving (Show)

-- | двигает точку на заданное расстояние по каджой из координат
--
move :: Point -> Double -> Double -> Point
move point dx dy = point {x = x point + dx, y = y point + dy}

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Double
dist p1 p2 = sqrt ((x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2)

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
  Node 1 
    (Node 2
      (Node 3 Leaf Leaf) 
      (Node 4 Leaf Leaf)
    ) 
    (Node 2 
      (Node 3
        (Node 4 Leaf Leaf) 
        (Node 5 Leaf Leaf)
      ) 
      Leaf
    )

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--
isPresented :: Int -> IntTree -> Bool
isPresented x Leaf = False
isPresented x node = 
  if (nodeValue node == x)
   then True
   else (isPresented x (leftChild node)) || (isPresented x (rightChild node))

testIsPresented1 :: Bool
testIsPresented1 = isPresented 1 binTreeOfInts == True

testIsPresented2 :: Bool
testIsPresented2 = isPresented 0 binTreeOfInts == False

testIsPresented3 :: Bool
testIsPresented3 = isPresented 5 binTreeOfInts == True
