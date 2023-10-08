-- 1. Реализуйте логическое И как функцию и как оператор (выберете произвольные символы для вашего оператора).

-- Функция
myAnd :: Bool -> Bool -> Bool
myAnd True True  = True
myAnd True False = False
myAnd False True = False
myAnd False False =  False


-- Оператор
infixr 3 #$#
(#$#) :: Bool -> Bool -> Bool
True #$# True = True
True #$# False = False
False #$# True = False
False #$# False =  False


-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib(n-1) + fib(n-2)
  
-- | c хвостовой рекурсией
--
fibTail :: Int -> Int
fibTail n = helpFunction n 0 1
  where helpFunction :: Int -> Int -> Int -> Int
        helpFunction n currentFib nextFib 
          | n == 0 = currentFib
          | otherwise = helpFunction (n-1) nextFib (currentFib + nextFib)

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?
-- Без хвостовой рекурсии с n = 1000 у меня даже не досчиаталось, а с хвостовой посчиталось мгновенно. 
-- Это и логично, так как хвостовая рекурасия помогает оптимизировать рекурсивный вызов функций

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
--myLength :: [a] -> Int
--myLength [] = 0
--myLength (_:xs) = 1 + myLength xs

myLength :: [a] -> Int
myLength xs = myLength_help xs 0
  where
    myLength_help [] n = n
    myLength_help (_:xs) n = myLength_help xs (n+1)


-- | возвращает хвост списка
--
myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_:xs) = Just xs

-- | возвращает список без последнего элемента
--
-- с библиотечной функцией
--myInit :: [a] -> Maybe [a]
--myInit [] = Nothing
--myInit xs = Just (init xs)
--
-- Без библиотечной функции
myInit :: [a] -> Maybe [a]
myInit [] = Nothing
myInit [_] = Just []
myInit (x:xs) = fmap (x:) (myInit xs)


-- | объединяет 2 списка
--
-- с библиотечной функцией
--myAppend :: [a] -> [a] -> [a]
--myAppend [] [] = Nothing
--myAppend xs ys = xs ++ ys
--
-- Без библиотечной функции
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- | разворачивает список
--
myReverse :: [a] -> [a]
myReverse = helpFunc []
  where
    helpFunc rev [] = rev
    helpFunc rev (x:xs) = helpFunc (x:rev) xs

-- | выдаёт элемент списка по индексу
elemByIndex :: Int ->[a] -> Either  String a
elemByIndex _ [] = Left "Error!"
elemByIndex i xs
  | i<0 = Left "Error!"
  | otherwise = case drop i xs of 
                [] -> Left "Error!"
                x:_ -> Right x 

testMyLength1 :: Bool
testMyLength1 = myLength [1..5] == 5

testMyLength2 :: Bool
testMyLength2 = myLength [] == 0

testMyLength3 :: Bool
testMyLength3 = myLength [1] == 1

testMyTail1 :: Bool
testMyTail1 = myTail [1..5] == Just [2..5]

testMyTail2 :: Bool
testMyTail2 = myTail ([] :: [Int]) == Nothing

testMyTail3 :: Bool
testMyTail3 = myTail [1] == Just []

testMyInit1 :: Bool
testMyInit1 = myInit [1..5] == Just [1..4]

testMyInit2 :: Bool
testMyInit2 = myInit ([] :: [Int]) == Nothing

testMyInit3 :: Bool
testMyInit3 = myInit [1] == Just []

testMyAppend1 :: Bool
testMyAppend1 = myAppend [1, 2, 3] [4, 5] == [1..5]

testMyAppend2 :: Bool
testMyAppend2 = myReverse ([] :: [Int]) == []

testMyAppend3 :: Bool
testMyAppend3 = myAppend [1, 2, 3] [] == [1, 2, 3]

testMyReverse1 :: Bool
testMyReverse1 = myReverse [1..5] == [5, 4, 3, 2, 1]

testMyReverse2 :: Bool
testMyReverse2 = myReverse ([] :: [Int]) == []

testMyReverse3 :: Bool
testMyReverse3 = myReverse [1] == [1]

testElemByIndex1 :: Bool
testElemByIndex1 = elemByIndex 1 [1..5] == Right 2

testElemByIndex2 :: Bool
testElemByIndex2 = elemByIndex 0 [1, 2, 3, 4, 5] == Right 1

testElemByIndex3 :: Bool
testElemByIndex3 = elemByIndex (-1) [1, 2, 3, 4, 5] == Left "Error!"


-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither f g (Left a) = Left (f a)
mapEither f g (Right b) = Right (g b)

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)

mapList :: (a -> b) -> [a] -> [b]
mapList f [] = []
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
chSucc Zero = Succ Zero
chSucc a = Succ a

chAdd :: ChurchNumber -> ChurchNumber -> ChurchNumber
chAdd Zero a = a
chAdd (Succ m) n = chSucc (chAdd m n)

chMult :: ChurchNumber -> ChurchNumber -> ChurchNumber
chMult Zero a = Zero
chMult (Succ m) n =  chAdd n (chMult m n)

chPow :: ChurchNumber -> ChurchNumber -> ChurchNumber
chPow _ Zero = chSucc Zero
chPow m (Succ n) = chMult (chPow m n) m 

-- !!!!!!!!!!!!!!!!!!!!!!
chPrev :: ChurchNumber -> ChurchNumber
chPrev Zero = Zero
chPrev (Succ a) = a

-- | Тесты могут выглядеть так
--
testChSucc :: Bool
testChSucc = chSucc churchTwo == churchThree
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

testChSucc2 :: Bool
testChSucc2 = chSucc churchThree == churchFour
  where
    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchFour :: ChurchNumber
    churchFour = Succ (Succ (Succ (Succ Zero)))

testChAdd :: Bool
testChAdd = chAdd churchTwo churchThree == churchFive
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))
    
    churchFive :: ChurchNumber
    churchFive = Succ (Succ (Succ (Succ (Succ Zero))))

testChAdd2 :: Bool
testChAdd2 = chAdd churchTwo churchTwo == churchFour
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchFour :: ChurchNumber
    churchFour = Succ (Succ (Succ (Succ Zero)))


testChMult :: Bool
testChMult = chMult churchTwo churchThree == churchSix
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchSix :: ChurchNumber
    churchSix = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

testChMult2 :: Bool
testChMult2 = chMult churchTwo churchTwo == churchFour
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchFour :: ChurchNumber
    churchFour = Succ (Succ (Succ (Succ Zero)))

testChPow :: Bool
testChPow = chPow churchTwo churchThree == churchEight
  where
    churchTwo :: ChurchNumber
    churchTwo = chSucc (chSucc Zero)

    churchThree :: ChurchNumber
    churchThree = chAdd churchTwo (chSucc Zero)

    churchEight :: ChurchNumber
    churchEight = chAdd (chMult churchTwo churchTwo) (chMult churchTwo churchTwo)

testChPow2 :: Bool
testChPow2 = chPow churchTwo churchTwo == churchFour
  where
    churchTwo :: ChurchNumber
    churchTwo = chSucc (chSucc Zero)

    churchFour :: ChurchNumber
    churchFour = chAdd (chMult churchTwo churchTwo) (chMult churchTwo churchTwo)


testChPrev :: Bool
testChPrev = chPrev churchThree == churchTwo
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

testChPrev2 :: Bool
testChPrev2 = chPrev churchFour == churchThree
  where
    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchFour :: ChurchNumber
    churchFour = Succ (Succ (Succ (Succ Zero)))

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

data Point = Point{
    x :: Double,
    y :: Double
}
 deriving (Show)

-- | двигает точку на заданное расстояние по каджой из координат
--
move :: Point -> Double -> Double -> Point
move (Point x y) x_shift y_shift = Point {x = x + x_shift, y = y + y_shift}

-- | возвращает дистанцию между 2 точками
--
dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt ((x1-x2)^2 + (y1 - y2)^2) 

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
binTreeOfInts = Node 2 (Node 3 Leaf Leaf) (Node 43 Leaf Leaf)

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--
isPresented :: Int -> IntTree -> Bool
isPresented _ Leaf = False
isPresented x (Node y left right)
  | x == y = True
  | otherwise = isPresented x left || isPresented x right


-- tests
testIsPresented1 :: Bool
testIsPresented1 = isPresented 1 binTreeOfInts == True
  where
    binTreeOfInts :: IntTree
    binTreeOfInts = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)

testIsPresented2 :: Bool
testIsPresented2 = isPresented 4 binTreeOfInts == False
  where
    binTreeOfInts :: IntTree
    binTreeOfInts = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)


main :: IO ()
main = do
  putStrLn "Task 4"
  putStrLn " "

  putStrLn "testMyLength1"
  print testMyLength1

  putStrLn "testMyLength2"
  print testMyLength2

  putStrLn "testMyLength3"
  print testMyLength3

  putStrLn "testMyTail1"
  print testMyTail1

  putStrLn "testMyTail2"
  print testMyTail2

  putStrLn "testMyTail3"
  print testMyTail3

  putStrLn "testMyInit1"
  print testMyInit1

  putStrLn "testMyInit2"
  print testMyInit2

  putStrLn "testMyInit3"
  print testMyInit3

  putStrLn "testMyAppend1"
  print testMyAppend1

  putStrLn "testMyAppend2"
  print testMyAppend2

  putStrLn "testMyAppend3"
  print testMyAppend3

  putStrLn "testMyReverse1"
  print testMyReverse1

  putStrLn "testMyReverse2"
  print testMyReverse2

  putStrLn "testMyReverse3"
  print testMyReverse3

  putStrLn "testElemByIndex1"
  print testElemByIndex1

  putStrLn "testElemByIndex2"
  print testElemByIndex2

  putStrLn "testElemByIndex3"
  print testElemByIndex3

  putStrLn " "

  putStrLn "Task 6"

  putStrLn " "

  putStrLn "testChSucc"
  print testChSucc

  putStrLn "testChSucc2"
  print testChSucc2

  putStrLn "testChAdd"
  print testChAdd

  putStrLn "testChAdd2"
  print testChAdd2

  putStrLn "testChMult"
  print testChMult

  putStrLn "testChMult2"
  print testChMult2

  putStrLn "testChPow"
  print testChPow

  putStrLn "testChPow2"
  print testChPow2

  putStrLn "testChPrev"
  print testChPrev

  putStrLn "testChPrev2"
  print testChPrev2

  putStrLn " "

  putStrLn "Task 8"

  putStrLn " "

  putStrLn "testIsPresented1"
  print testIsPresented1

  putStrLn "testIsPresented2"
  print testIsPresented2





-------------------------------------------------------------------------------