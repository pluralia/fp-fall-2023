-- 1. Реализуйте логическое И как функцию и как оператор (выберете произвольные символы для вашего оператора).

-- 1.1 function
myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

-- 1.2 operator
infix 1 *$*
(*$*) :: Bool -> Bool -> Bool
True *$* True = True
_ *$* _ = False

-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | 2.1 без хвостовой рекурсии
--
fib :: Int -> Integer
fib n | n == 1 = 1
      | n == 2 = 1
      | otherwise = fib(n - 1) + fib(n - 2)

-- Tests for 2.1
-- fib 12 -> 144
-- fib 1200 -> очень долго, когда я хотела абортировать выполнение, все сломалось. Видимо, как-то не так остановила

-- | 2.2 c хвостовой рекурсией
--
fibTail :: Int -> Integer -- у меня очень странное число получалось, я погуглила что может вместить больше
fibTail n = helper n 1 1
  where
    helper 1 a _ = a
    helper 2 _ b = b
    helper n a b = helper (n - 1) b (a + b)

-- Test for 2.2
-- fibTail 12 -> считает одинакого с 2.1, все ок
-- fibTail 1200 -> 27269884455406270157991615313642198705000779992917725821180502894974726476373026809482509284562310031170172380127627214493597616743856443016039972205847405917634660750474914561879656763268658528092195715626073248224067794253809132219056382939163918400

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?
-- fibTail работает очень быстро, а fib не отрабатывет большие числа (допускаю, что если бы я дала доработать, то была бы
-- ошибка переполнения стека)
-- На лекции мы обсуждали, что в haskell хвостовая рекурсия эфективно преобразуется в цикл
-------------------------------------------------------------------------------

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- 3.1| превращает некаррированную функцию в каррированную
--   Подсказка: `((a, b) -> c) -> a -> b -> c` эквивалентно `((a, b) -> c) -> (a -> b -> c)`
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f(x, y)

-- Test for 3.1

-- Некаррированная функция, которая возвращает * двух чисел
uncMult :: (Int, Int) -> Int
uncMult (x, y) = x * y

-- myCurry uncMult 2 6 -> 12

-- 3.2| превращает каррированную функцию в некаррированную
--   Подсказка: `(a -> b -> c) -> (a, b) -> c` эквивалентно `(a -> b -> c) -> ((a, b) -> c)`
--
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f(x, y) = f x y

-- Test for 3.2
-- Каррированная функция, которая возвращает * двух чисел
curMult :: Int -> Int -> Int
curMult x y = x * y
-- myUncurry curMult (2, 6) -> 12

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- 4.1| возвращает длину списка
--
myLength :: [a] -> Int
myLength list = myLengthHelper list 0
  where
    myLengthHelper :: [a] -> Int -> Int
    myLengthHelper [] acc = acc
    myLengthHelper (x : xs) acc = myLengthHelper xs (acc + 1)

-- myLength [0..1000000]->1000001

-- 4.2| возвращает хвост списка
--
myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_ : xs) = Just xs

-- myTail [1,3,5,7,9] -> Just [3,5,7,9]
-- myTail [9] -> Just []
-- myTail [] -> Nothing

-- 4.3| возвращает список без последнего элемента
--
myInit :: [a] -> Either String [a]
myInit [] = Left "Empty list"
myInit [x] = Right []
myInit (x : xs) = Right (x : myInitRes)
  where
    Right myInitRes = myInit xs

-- myInit [1,3,5,7,9] -> Right [1,3,5,7]
-- myInit [9] -> Right []
-- myInit [] -> Left "Empty list"

-- 4.4| объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend x [] = x
myAppend [] y = y 
myAppend (x : xs) y = x : myAppend xs y

-- myAppend [] [] -> []
-- myAppend [] [1,2,3,4] -> [1,2,3,4]
-- myAppend [1,2,3,4] [] -> [1,2,3,4]
-- myAppend [1,2,3,4] [1..10] -> [1,2,3,4,1,2,3,4,5,6,7,8,9,10]

-- 4.5| разворачивает список
-- 
myReverse :: [a] -> Either String [a]
myReverse xs = myReverseHelper xs []
  where
    myReverseHelper :: [a] -> [a] -> Either String [a]
    myReverseHelper [] acc = Right acc
    myReverseHelper (x : xs) acc = myReverseHelper xs (x : acc)

-- myReverse ['a','1','%'] -> Right "%1a"
-- myReverse ['a'] -> Right "a"
-- myReverse [] -> Right []

-- 4.6| выдаёт элемент списка по индексу
--
elemByIndex :: [a] -> Int -> Either String a
elemByIndex [] _ = Left "No elements"
elemByIndex _ 0 = Left "Index must be > 0"
elemByIndex (x : _) 1 = Right x
elemByIndex (x : xs) n | n < 0 = Left "Index must be > 0"
                       | otherwise = elemByIndex xs (n - 1)

-- elemByIndex [1,3,5,7,9] 3 -> Right 5
-- elemByIndex [1,3,5,7,9] 0 -> Left "Index must be > 0"
-- elemByIndex [] 3 -> Left "No elements"

-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither f _ (Left a) = Left (f a)
mapEither _ g (Right b) = Right (g b)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

mapList :: (a -> b) -> [a] -> [b]
mapList f xs = mapListHelp f xs []
  where
    mapListHelp :: (a -> b) -> [a] -> [b] -> [b]
    mapListHelp _ [] acc = acc
    mapListHelp f (x:xs) acc = mapListHelp f xs (acc ++ [f x])


-------------------------------------------------------------------------------

-- 6. Тип 'ChurchNumber' является представлением чисел Чёрча в Haskell.

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show, Eq)

-- | Видно, что, например, число три, закодированное с помощью типа 'ChurchNumber',
--   является трёхкратным применением конструктора данных 'Succ' (который тоже функция) к 'Zero'
--
-- Для тестиков:
churchZero :: ChurchNumber
churchZero = Zero

churchOne :: ChurchNumber
churchOne = Succ Zero

churchTwo :: ChurchNumber
churchTwo = Succ (Succ Zero)

churchThree :: ChurchNumber
churchThree = Succ (Succ (Succ Zero))

churchFour :: ChurchNumber
churchFour = Succ(Succ (Succ (Succ Zero)))

churchFive :: ChurchNumber
churchFive = Succ(Succ (Succ (Succ (Succ Zero))))

churchSix :: ChurchNumber
churchSix = Succ(Succ (Succ (Succ (Succ (Succ Zero)))))

churchEight :: ChurchNumber
churchEight = Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero)))))))
-------------------------------------------------------

-- Реализуйте функции `succ`, `add`, `mult`, `pow` и `prev` для типа 'ChurchNumber'
--    - укажите типы функций
--    - напишите тесты

chSucc :: ChurchNumber -> ChurchNumber
chSucc n = Succ n

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd Zero x = x
chAdd (Succ x) y = Succ (chAdd x y)

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult _ Zero = Zero
chMult Zero _ = Zero
chMult x (Succ y) = chAdd x (chMult x y)

chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow _ Zero = Succ Zero 
chPow x (Succ y) = chMult x (chPow x y)

chPrev :: ChurchNumber -> ChurchNumber
chPrev Zero = Zero
chPrev (Succ x) = x

-- | Тесты 

-- ChSucc
testChSucc :: Bool
testChSucc = chSucc churchFive == churchSix

-- ChAdd 
testChAdd1 :: Bool
testChAdd1 = chAdd churchTwo churchFour == churchSix

testChAdd2 :: Bool
testChAdd2 = chAdd churchZero churchThree == churchThree

-- ChMult 
testChMult1 :: Bool
testChMult1 = chMult churchTwo churchFour == churchEight

testChMult2 :: Bool
testChMult2 = chMult churchZero churchFive == churchZero

-- СhPow 
testChPow1 :: Bool
testChPow1 = chPow churchTwo churchThree == churchEight

testChPow2 :: Bool
testChPow2 = chPow churchThree churchZero == churchOne

-- ChPrev 
testChPrev1 :: Bool
testChPrev1 = chPrev churchThree == churchTwo 

testChPrev2 :: Bool
testChPrev2 = chPrev churchZero == churchZero

--
testCh = and [
      testChSucc, 
      testChAdd1, testChAdd2, 
      testChMult1, testChMult2, 
      testChPow1, testChPow2, 
      testChPrev1, testChPrev2
    ]

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

data Point = Point {
    x :: Double,
    y :: Double
}
  deriving (Show, Eq)

-- | двигает точку на заданное расстояние по каджой из координат
--
move :: Point -> Double -> Double -> Point
move (Point {x, y}) dx dy = Point {x = x + dx, y = y + dy}

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Double
dist p1 p2 = sqrt ((x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2)

-- Tests just in case
-- move
testMove :: Bool
testMove = movedPoint == expectedPoint
  where
    -- Начальные координаты
    start :: Point
    start = Point {x = 0.0,
                   y = 3.0}

    -- Расстояние для перемещения по каждой из координат
    dx, dy :: Double
    dx = 6.0
    dy = -1.0

    expectedPoint :: Point
    expectedPoint = Point {x = 6.0,
                           y = 2.0}

    -- само перемещение
    movedPoint :: Point
    movedPoint = move start dx dy

-- dist
testDist = distance == expectedDist
  where
    -- Точки
    p1 = Point{x = 0.0,
               y = 3.0}
    p2 = Point{x = 4.0,
               y = 0.0}
    -- тест-ответ
    expectedDist :: Double
    expectedDist = 5.0
    -- ответ
    distance :: Double
    distance = dist p1 p2

testPoints = and [testMove, testDist]

-------------------------------------------------------------------------------

-- 8. Бинарное дерево может быть задано следующим образом: -

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
binTreeOfInts = undefined

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--
-- isPresented :: Int -> IntTree -> Bool
isPresented = undefined

-------------------------------------------------------------------------------