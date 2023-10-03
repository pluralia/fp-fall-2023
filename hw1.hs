-- import Test.HUnit  -- это фрейворк для тестирования в Хаскеле (спасибо гугл)


-- 1. Реализуйте логическое И как функцию и как оператор (выберете произвольные символы для вашего оператора).

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _ = False

(/\||/\*) :: Bool -> Bool -> Bool
True /\||/\* True = True
_ /\||/\* _ = False


-- -------------------------------------------------------------------------------
-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
--  fib :: Int -> Int
--  fib 0 = 0
--  fib 1 = 1
--  fib n = fib (n-1) + fib (n-2)


-- | c хвостовой рекурсией
--
fibTail :: Int -> Integer
fibTail n = go n 0 1
    where 
        go 0 a _ = a
        go n a b = go (n-1) b (a+b)


-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?

-- Запустив функцию fib 1000 я ничего не получил, потому что мой компьютер всё никак не мог досчитать
-- результат, вероятно, это из-за того, что функция fib рекурсивно запускается от всё меньших и 
-- меньших значений, при этом, по сути, ничего не вычисляя, ведь вычислять она будет когда дойдет
-- до чисел 0 1 в самом низу стека вызовов рекурсии (как было в примере из лекции/семинара).

-- Запустив функцию fibTail 1000 я получил 817770325994397771, а запустив fibTail 1500 я получил -5273436160863865264
-- видимо из-за выхода за пределы мощности вычислений. Но т.к. вычисления происходят сразу, то и результат
-- отображается мгновенно.


-- -------------------------------------------------------------------------------
-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--   Подсказка: `((a, b) -> c) -> a -> b -> c` эквивалентно `((a, b) -> c) -> (a -> b -> c)`
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry f a b = f (a, b)

-- | превращает каррированную функцию в некаррированную
--   Подсказка: `(a -> b -> c) -> (a, b) -> c` эквивалентно `(a -> b -> c) -> ((a, b) -> c)`
--
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry f (a, b) = f a b

-- -- пример работы myCurry
-- add :: (Int, Int) -> Int
-- add (x, y) = x + y

-- curriedAdd :: Int -> Int -> Int
-- curriedAdd = myCurry add

-- result1 = curriedAdd 2 3 -- результат: 5

-- -- пример работы myUncurry
-- multiply :: Int -> Int -> Int
-- multiply x y = x * y

-- uncurriedMultiply :: (Int, Int) -> Int
-- uncurriedMultiply = myUncurry multiply

-- result2 = uncurriedMultiply (4, 5) -- результат: 20

-- -------------------------------------------------------------------------------
-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- | возвращает длину списка
-- ЭТО ПРЯМАЯ РЕКУРСИЯ, ЛОМАЕТСЯ НА БОЛЬШИХ ЧИСЛАХ 
-- myLength :: [a] -> Int
-- myLength [] = 0
-- myLength (_:xs) = 1 + myLength xs  -- мы как обы отщипываем 1 кусок от списка и запускаемся с len-1

-- А ЭТО ХВОСТОВАЯ РЕКУРСИЯ, РАБОТАЕТ ВСЕГДА
myLengthTail :: [a] -> Integer
myLengthTail xs = myLengthTailHelper xs 0
  where
    myLengthTailHelper [] acc = acc
    myLengthTailHelper (_:xs) acc = myLengthTailHelper xs (acc + 1)
-- по идее оно должно работать, но я ловлю какой-то странный баг, связанный с объемом файлов подкачки на моем компьютере
-- <interactive>: osCommitMemory: VirtualAlloc MEM_COMMIT failed to commit 1048576 bytes of memory  (error code: 1455): Файл подкачки слишком мал для завершения операции.

-- | возвращает хвост списка
--
myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_:xs) = Just xs  -- просто вернули список без начала

-- | возвращает список без последнего элемента
--
-- myInit :: [a] -> Maybe [a]
-- myInit [] = Nothing
-- myInit [x] = Just []
-- myInit (x:xs) = Just (x : fromJust (myInit xs))  -- дойдем рекурсивно до конца списка и не возьмем последний элемент
--   where fromJust (Just x) = x

-- ПЕРЕПИСАЛ ФУНКЦИЮ ЧЕРЕЗ mapMaybe
myInitTail :: [a] -> Maybe [a]
myInitTail xs = myInitTailHelper xs Nothing
  where
    myInitTailHelper :: [a] -> Maybe [a] -> Maybe [a]
    myInitTailHelper [] acc = acc
    myInitTailHelper [x] acc = Just []
    myInitTailHelper (x:xs) acc = myInitTailHelper xs (mapMaybe (\_ -> x) acc)

-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys  -- просто рекурсивно мерджим два списка поэлементно 

-- | разворачивает список
--
-- myReverse :: [a] -> [a]
-- myReverse [] = []
-- myReverse (x:xs) = myAppend (myReverse xs) [x]  -- разворачиваем список поэлементно и мерджим

-- а теперь через хвостовую рекурсию!
-- т.е. аккумуляторный список как бы "накапливает" элементы исходного списка в обратном порядке
myReverseTail :: [a] -> [a]
myReverseTail xs = myReverseTailHelper xs []
  where
    myReverseTailHelper :: [a] -> [a] -> [a]
    myReverseTailHelper [] acc = acc
    myReverseTailHelper (x:xs) acc = myReverseTailHelper xs (x:acc)

-- | выдаёт элемент списка по индексу
elemByIndex :: Int -> [a] -> Either String a
elemByIndex _ [] = Left "Index out of range"
elemByIndex n (x:xs)  -- уменьшаем n до нуля, идя по списку, где n==0, там и наш элемент по индексу лежит
  | n == 0 = Right x
  | n < 0 = Left "Negative index"
  | otherwise = elemByIndex (n-1) xs


-- -- ТЕСТЫ


-- -- тесты на хаскеле пишутся (согласно тому же гуглу) по следующей схеме (+- похожи на питоновские assert)
-- -- сначала объявляется ключевое слово TestCase, который выдаст либо войд резалт, либо assertFailure
-- -- затем пишется к чему оно применяется в скобочках, там идет функция проверки 
-- -- assertEqual которая принимает три аргумента: строка, которая отобразится при падении теста, ожидаемое значение
-- -- и реальное значение (сюда пишем функцию и к чему ее применяем)

-- -- тесты для функции myLength
-- testMyLengthEmptyList = TestCase (assertEqual "for []" 0 (myLength []))
-- testMyLengthNonEmptyList = TestCase (assertEqual "for [1,2,3,4]" 4 (myLength [1,2,3,4]))

-- -- тесты для функции myTail
-- testMyTailEmptyList = TestCase (assertEqual "for []" Nothing (myTail []))
-- testMyTailNonEmptyList = TestCase (assertEqual "for [1,2,3,4]" (Just [2,3,4]) (myTail [1,2,3,4]))

-- -- тесты для функции myInit
-- testMyInitEmptyList = TestCase (assertEqual "for []" Nothing (myInit []))
-- testMyInitSingleElementList = TestCase (assertEqual "for [1]" (Just []) (myInit [1]))
-- testMyInitNonEmptyList = TestCase (assertEqual "for [1,2,3,4]" (Just [1,2,3]) (myInit [1,2,3,4]))

-- -- тесты для функции myAppend
-- testMyAppendEmptyLists = TestCase (assertEqual "for [] []" [] (myAppend [] []))
-- testMyAppendFirstListEmpty = TestCase (assertEqual "for [] [1,2,3]" [1,2,3] (myAppend [] [1,2,3]))
-- testMyAppendSecondListEmpty = TestCase (assertEqual "for [1,2,3] []" [1,2,3] (myAppend [1,2,3] []))
-- testMyAppendNonEmptyLists = TestCase (assertEqual "for [1,2] [3,4]" [1,2,3,4] (myAppend [1,2] [3,4]))

-- -- тесты для функции myReverse
-- testMyReverseEmptyList = TestCase (assertEqual "for []" [] (myReverse []))
-- testMyReverseNonEmptyList = TestCase (assertEqual "for [1,2,3,4]" [4,3,2,1] (myReverse [1,2,3,4]))

-- -- тесты для функции elemByIndex
-- testElemByIndexEmptyList = TestCase (assertEqual "for 0 []" (Left "Index out of range") (elemByIndex 0 []))
-- testElemByIndexNegativeIndex = TestCase (assertEqual "for -1 [1,2,3]" (Left "Negative index") (elemByIndex (-1) [1,2,3]))
-- testElemByIndexOutOfRange = TestCase (assertEqual "for 5 [1,2,3]" (Left "Index out of range") (elemByIndex 5 [1,2,3]))
-- testElemByIndexValidIndex = TestCase (assertEqual "for 2 [1,2,3]" (Right 3) (elemByIndex 2 [1,2,3]))

-- -- запуск тестов
-- main :: IO ()
-- -- runTestTT это функция, которая принимает один тест или их список 
-- -- ($ это оператор применения, не уверен, что он тут сильно нужен, но в гугле было так, так что оставил, ибо не трогай, пока работает)
-- main = runTestTT $ TestList [  
--   testMyLengthEmptyList,
--   testMyLengthNonEmptyList,
--   testMyTailEmptyList,
--   testMyTailNonEmptyList,
--   testMyInitEmptyList,
--   testMyInitSingleElementList,
--   testMyInitNonEmptyList,
--   testMyAppendEmptyLists,
--   testMyAppendFirstListEmpty,
--   testMyAppendSecondListEmpty,
--   testMyAppendNonEmptyLists,
--   testMyReverseEmptyList,
--   testMyReverseNonEmptyList,
--   testElemByIndexEmptyList,
--   testElemByIndexNegativeIndex,
--   testElemByIndexOutOfRange,
--   testElemByIndexValidIndex
--   ]


-- -------------------------------------------------------------------------------
-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither f _ (Left x) = Left (f x)
mapEither _ g (Right y) = Right (g y)

-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- mapMaybe _ Nothing = Nothing
-- mapMaybe f (Just x) = Just (f x)

-- Реализация через хвостовую рекурсию

mapMaybeTail :: (a -> b) -> Maybe a -> Maybe b
mapMaybeTail f x = mapMaybeHelper f x Nothing
  where
    mapMaybeHelper :: (a -> b) -> Maybe a -> Maybe b -> Maybe b
    mapMaybeHelper _ Nothing acc = acc
    mapMaybeHelper f (Just x) acc = mapMaybeHelper f Nothing (Just (f x):acc)

mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs


-- -------------------------------------------------------------------------------
-- 6. Тип 'ChurchNumber' является представлением чисел Чёрча в Haskell. 
-- я что-то сделал, но без тестов и в целом задание не очень понятное, честно говоря
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

-- | The successor of a Church numeral
chSucc :: ChurchNumber -> ChurchNumber
chSucc = Succ

-- | Addition of two Church numerals
chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd (Succ m) n = chAdd m (chSucc n)
chAdd Zero n = n

-- | Multiplication of two Church numerals
chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult (Succ m) n = chAdd n (chMult m n)
chMult Zero n = Zero

-- | Raising to the power of a Church numeral
chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow Zero n = chSucc Zero
chPow (Succ m) n = chMult n (chPow m n)


-- | Previous of a Church numeral
chPrev :: ChurchNumber -> ChurchNumber
chPrev (Succ (Succ n)) = Succ n
chPrev _ = Zero

-- | Тесты могут выглядеть так
--
-- testChSucc :: Bool
-- testChSucc = chSucc churchTwo == churchThree
--   where
--     churchTwo :: ChurchNumber
--     churchTwo = Succ (Succ Zero)


-- ТЕСТЫ

testChSucc :: Bool
testChSucc = chSucc churchTwo == churchThree

testChAdd1 :: Bool
testChAdd1 = chAdd churchTwo churchThree == churchFive

testChAdd2 :: Bool
testChAdd2 = chAdd churchZero churchThree == churchThree

testChMult1 :: Bool
testChMult1 = chMult churchTwo churchThree == churchSix

testChMult2 :: Bool
testChMult2 = chMult churchZero churchThree == churchZero

testChPow1 :: Bool
testChPow1 = chPow churchTwo churchThree == churchEight

testChPow2 :: Bool
testChPow2 = chPow churchThree churchZero == churchOne

testChPrev1 :: Bool
testChPrev1 = chPrev churchThree == churchTwo 

testChPrev2 :: Bool
testChPrev2 = chPrev churchZero == churchZero

testAll = and [
      testChSucc, testChAdd1, testChAdd2, testChMult1, testChMult2, testChPow1, testChPow2, testChPrev1, testChPrev2
    ]

-- -------------------------------------------------------------------------------
-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

-- Определение типа данных Point
data Point = Point { x :: Double, y :: Double }  -- создали тип данных как на практике

-- | двигает точку на заданное расстояние по каждой из координат
--
move :: Double -> Double -> Point -> Point
move dx dy (Point x y) = Point (x + dx) (y + dy)  -- просто применили к элементам типа сдвиг

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- -------------------------------------------------------------------------------
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
-- я не совсем понял, нужно задавать дерево с вводом с клавиатуры или просто рандомное дерево
-- поэтому пошел по пути наименьшего сопротивления и просто прописал ноды вручную 
type IntTree = BinaryTree Int

binTreeOfInts :: IntTree
binTreeOfInts =
  Node 55
    (Node 33
      (Node 11 Leaf Leaf)
      (Node 44 Leaf Leaf)
    )
    (Node 88
      (Node 66 Leaf Leaf)
      (Node 100 Leaf Leaf)
    )

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--
isPresented :: Int -> IntTree -> Bool
isPresented _ Leaf = False
isPresented x (Node value left right)
  | x == value = True
  | x < value = isPresented x left
  | otherwise = isPresented x right


-- ТЕСТЫ

-- main :: IO ()
-- main = do
-- putStrLn $ show это команда в хаскеле для вывода на экран ЗНАЧЕНИЯ, ПОЛУЧЕННОГО ПОСЛЕ ПРИМЕНЕНИЯ ФУНКЦИИ
-- сама по себе putStrLn умеет выводить только строки, а $ нужен чтобы применять то, что справа к тому, что слева, грубо говоря
--   putStrLn $ show $ isPresented 55 binTreeOfInts -- True
--   putStrLn $ show $ isPresented 100 binTreeOfInts -- True
--   putStrLn $ show $ isPresented (-1) binTreeOfInts -- False


-- -------------------------------------------------------------------------------
