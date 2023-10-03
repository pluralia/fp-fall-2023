-- 1. Реализуйте логическое И как функцию и как оператор (выберете произвольные символы для вашего оператора).

-- Как функция:
myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

-- Tests 1.1
-- myAnd True True => True
-- myAnd False True => False
-- myAnd True False => False

-- Как бинарный оператор:
-- 
infixl 3 &&&
(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_ &&& _ = False


-- Tests 1.2
-- True &&& True => True
-- False &&& True => False
-- True &&& False => False

-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Int -> Integer
fib n | n == 1    = 1
      | n == 2    = 1
      | otherwise = fib (n - 1) + fib (n - 2)

-- Tests 2.1
-- fib 1200 => результатов так и не дождался
-- fib 1800 => результатов так и не дождался
-- fib 2200 => результатов так и не дождался

-- | c хвостовой рекурсией
--
fibTail :: Int -> Integer
fibTail n = helper n 1 1
  where
    helper 1 a _ = a        -- a == (n - 1)
    helper 2 _ b = b        -- b == (n - 2)
    helper n a b = helper (n - 1) b (a + b)

-- Tests 2.2
-- fibTail 1200 => 27269884455406270157991615313642198705000779992917725821180502894974726476373026809482509284562310031170172380127627214493597616743856443016039972205847405917634660750474914561879656763268658528092195715626073248224067794253809132219056382939163918400
-- fibTail 1800 => 6733912172802933472606353001846945074658287378884326089477601632746080275952604203199580265153593862390858117766432295498560989719530281829452850286454536277301941625978000791367655413469297462257623927534855511388238610890658838439857922737938956952361558179389004339772497124977152035343580348215676156404424782380266118900316342135562815217465023272599528784782167145877600
-- fibTail 2200 => 2650473996596436536356196798944232122138617049795750388692937504820987398314396962165291928984759310943187283298303610029503030140838762839122828018658103022518383286820431142237783429918908794457401138893192264119726140265930740092020458651842019276316961265150890061616625689188717302632243432890033319431457844402580536768596104366856103703091630247436446218920974144804980906861110942449601588371061780928081985120405866730904463678192727714343144775647275

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?
--
-- Версия с хвостовой рекурсией (ХР) работает практически мгновенно,
-- а первая версия так и не закончила работу спустя 20 минут (скорее всего дальше был бы стэковерфлоу, но было некогда ждать).
-- Такая разница во времени, как было сказано на лекции, благодаря тому, что компилятор умеет эффективно превращать ХР в цикл
-------------------------------------------------------------------------------

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--   Подсказка: `((a, b) -> c) -> a -> b -> c` эквивалентно `((a, b) -> c) -> (a -> b -> c)`
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f x y = f(x, y)

-- | превращает каррированную функцию в некаррированную
--   Подсказка: `(a -> b -> c) -> (a, b) -> c` эквивалентно `(a -> b -> c) -> ((a, b) -> c)`
--
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f(x, y) = f x y 


-- Tests 3.1
-- Некаррированная функция, которая возвращает сумму двух чисел
uncAdd :: (Int, Int) -> Int
uncAdd (x, y) = x + y

-- myCurry uncAdd 2 3 => 5

-- Tests 3.2
-- Каррированная функция, которая возвращает сумму двух чисел
curAdd :: Int -> Int -> Int
curAdd x y = x + y

-- myUncurry curAdd (2, 3) => 5

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- Определение списка:
-- data [a] = [] | a : [a]

-- | возвращает длину списка
--
myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

-- Tests 4.1
-- myLength [1, 2, 3, 4] => 4
-- myLength [1] => 1
-- myLength [] => 0

-- | возвращает хвост списка
--
myTail :: [a] -> Maybe [a]
myTail []       = Nothing
myTail (_ : xs) = Just xs


-- Test 4.2
-- myTail [1, 2, 3, 4] => [2,3,4]
-- myTail [1] => []
-- myTail [] => *** Exception: The list is empty

-- | возвращает список без последнего элемента
--
myInit :: [a] -> Either String [a]
myInit []        = Left "The list is empty"
myInit [x]       = Right []
myInit (x : xs)  = Right (x : myInitResult)
  where
    Right myInitResult = myInit xs --------------------------------------------

-- Test 4.3
-- myInit [1, 2, 3, 4] => [1,2,3]
-- myInit [1] => []
-- myInit [] => *** Exception: The list is empty

-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend [] []      = []
myAppend x []       = x
myAppend [] y       = y 
myAppend (x : xs) y = x : myAppend xs y

-- Test 4.4
-- myAppend [1, 2, 3, 4] [2, 10] => [1,2,3,4,2,10]
-- myAppend [1, 2, 3, 4] [] => [1, 2, 3, 4]
-- myAppend [] [1, 2, 3, 4] => [1, 2, 3, 4]
-- myAppend [] [] => []

-- | разворачивает список
-- 
myReverse :: [a] -> Either String [a]
myReverse xs = myReverseHelper xs []
  where
    myReverseHelper :: [a] -> [a] -> Either String [a]
    myReverseHelper [] acc = Right acc
    myReverseHelper (x : xs) acc = myReverseHelper xs (x : acc)


-- Test 4.5
-- myReverse [1, 2, 3, 4] => [4,3,2,1]
-- myReverse [4] => [4]
-- myReverse [] => *** Exception: The list is empty

-- | выдаёт элемент списка по индексу
elemByIndex :: [a] -> Int -> Either String a
elemByIndex [] _                   = Left "Error"
elemByIndex _ 0                    = Left "Invalid index, index must be greater than 0"
elemByIndex (x : _) 1              = Right x
elemByIndex (x : xs) n | n < 0     = Left "Invalid index, index must be positive"
                       | otherwise = elemByIndex xs (n - 1)


-- Test 4.6
-- elemByIndex [1, 2, 3, 4] 3 => 3
-- elemByIndex [1, 2, 3, 4] 1 => 1
-- elemByIndex [1, 2, 3, 4] 0 => *** Exception: Invalid index, index must be greater than 0
-- elemByIndex [1, 2, 3, 4] (-2) => *** Exception: Invalid index, index must be positive
-- elemByIndex [] 2 => *** Exception: The list is empty
-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither f _ (Left a) = Left (f a)
mapEither _ g (Right b) = Right (g b)

-- Test 5.1
-- mapEither fib myLength (Left 10) =>  Left 55
-- mapEither fib myLength (Right [1, 2, 4]) => Right 3

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

-- Test 5.2
-- mapMaybe fib Nothing => Nothing
-- mapMaybe fib (Just 10) => Just 55

mapList :: (a -> b) -> [a] -> [b]
mapList f xs = mapListHelper f xs []
  where
    mapListHelper :: (a -> b) -> [a] -> [b] -> [b]
    mapListHelper _ [] acc = acc
    mapListHelper f (x:xs) acc = mapListHelper f xs (acc ++ [f x])


-- Test 5.3
-- mapList fib [3, 6] => [2,8]
-- mapList fib [] => []



-------------------------------------------------------------------------------

-- 6. Тип 'ChurchNumber' является представлением чисел Чёрча в Haskell.

data ChurchNumber = Zero | Succ ChurchNumber
  deriving (Show, Eq)

-- | Видно, что, например, число три, закодированное с помощью типа 'ChurchNumber',
--   является трёхкратным применением конструктора данных 'Succ' (который тоже функция) к 'Zero'

-----------------------Для тестов----------------------:
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
chAdd Zero x =  x
chAdd (Succ x) y = Succ (chAdd x y)

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult Zero _ = Zero
chMult _ Zero = Zero
chMult x (Succ y) = chAdd x (chMult x y)

chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow _ Zero = Succ Zero 
chPow x (Succ y) = chMult x (chPow x y)

chPrev :: ChurchNumber -> ChurchNumber
chPrev Zero = Zero
chPrev (Succ x) = x


-- | Тесты

-----------------------testChSucc-------------------------
testChSucc :: Bool
testChSucc = chSucc churchTwo == churchThree

-----------------------testChAdd-------------------------
testChAdd1 :: Bool
testChAdd1 = chAdd churchTwo churchThree == churchFive

testChAdd2 :: Bool
testChAdd2 = chAdd churchZero churchThree == churchThree

-----------------------testChMult-------------------------
testChMult1 :: Bool
testChMult1 = chMult churchTwo churchThree == churchSix

testChMult2 :: Bool
testChMult2 = chMult churchZero churchThree == churchZero

testChMult3 :: Bool
testChMult3 = chMult churchThree churchZero == churchZero

-----------------------testChPow--------------------------
testChPow1 :: Bool
testChPow1 = chPow churchTwo churchThree == churchEight

testChPow2 :: Bool
testChPow2 = chPow churchThree churchZero == churchOne

-----------------------testChPrev--------------------------
testChPrev1 :: Bool
testChPrev1 = chPrev churchThree == churchTwo 

testChPrev2 :: Bool
testChPrev2 = chPrev churchZero == churchZero
-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

data Point = Point {
    x :: Double,
    y :: Double
}
  deriving (Show, Eq)

-- | двигает точку на заданное расстояние по каждой из координат
--
move :: Point -> Double -> Double -> Point
move p dx dy = Point {x = x p + dx, y = y p + dy}

-----------------------testMovePoint--------------------------
testMovePoint :: Bool
testMovePoint = movedPoint == expectedPoint
  where
    -- Начальные координаты
    startPoint :: Point
    startPoint = Point {x = 1.0,
                        y = 2.0}

    -- Расстояние для перемещения по каждой из координат
    dx, dy :: Double
    dx = 3.0
    dy = -1.0

    expectedPoint :: Point
    expectedPoint = Point {x = 4.0,
                           y = 1.0}

    -- Наше перемещение
    movedPoint :: Point
    movedPoint = move startPoint dx dy

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Double
dist p1 p2 = sqrt ((x p1 - x p2) ^ 2 + (y p1 - y p2) ^ 2) 

-----------------------testdistBetweenPoints--------------------------
testdistBetweenPoints :: Bool
testdistBetweenPoints = distBetweenPoints == expectedDist
  where
    -- Первая точка
    point1 :: Point
    point1 = Point {x = 1.0,
                    y = 1.0}
    -- Вторая точка
    point2 :: Point
    point2 = Point {x = 4.0,
                    y = 1.0}
    -- Правильный ответ
    expectedDist :: Double
    expectedDist = 3.0
    
    -- Вычисляем ответ с помощью функции
    distBetweenPoints :: Double
    distBetweenPoints = dist point1 point2

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

type IntTree = BinaryTree Int

binTreeOfInts :: IntTree
binTreeOfInts = 
  Node 10 
    (Node 5 
      (Node 2 Leaf Leaf) 
      (Node 7 Leaf Leaf)
    ) 
    (Node 15 
      (Node 12 
        (Node 11 Leaf Leaf) 
        (Node 14 Leaf Leaf)
      ) 
      Leaf
    )

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--

isPresented :: Int -> IntTree -> Bool
isPresented _ Leaf = False 
isPresented target (Node value left right)
                                          | target == value = True
                                          | target < value = isPresented target left 
                                          | otherwise = isPresented target right

--------------------------------testIsPresented--------------------------------
testIsPresented :: Bool
testIsPresented = and
  [ isPresented 5 binTreeOfInts == True  -- Элемент 5 присутствует в дереве
  , isPresented 11 binTreeOfInts == True  -- Элемент 11 присутствует в дереве
  , isPresented 8 binTreeOfInts == False  -- Элемент 8 отсутствует в дереве
  ]
-------------------------------------------------------------------------------
