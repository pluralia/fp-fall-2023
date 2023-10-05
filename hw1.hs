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
fib :: Int -> Int
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
myInit :: [a] -> Maybe [a]
myInit [] = Nothing
myInit xs = Just (init xs)

-- | объединяет 2 списка
--
myAppend :: [a] -> [a] -> [a]
--myAppend [] [] = Nothing
myAppend xs ys = xs ++ ys

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

testMyLength :: Bool
testMyLength = myLength [1, 2, 3, 4, 5] == 5

testMyTail :: Bool
testMyTail = myTail [1, 2, 3, 4, 5] == Just [2, 3, 4, 5]

testMyInit :: Bool
testMyInit = myInit [1, 2, 3, 4, 5] == Just [1, 2, 3, 4]

testMyAppend :: Bool
testMyAppend = myAppend [1, 2, 3] [4, 5] == [1, 2, 3, 4, 5]

testMyReverse :: Bool
testMyReverse = myReverse [1, 2, 3, 4, 5] == [5, 4, 3, 2, 1]

testElemByIndex :: Bool
testElemByIndex = elemByIndex 1 [1, 2, 3, 4, 5] == Right 2


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

testChAdd :: Bool
testChAdd = chAdd churchTwo churchThree == churchFive
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))
    
    churchFive :: ChurchNumber
    churchFive = Succ (Succ (Succ (Succ (Succ Zero))))


testChMult :: Bool
testChMult = chMult churchTwo churchThree == churchSix
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))

    churchSix :: ChurchNumber
    churchSix = Succ (Succ (Succ (Succ (Succ (Succ Zero)))))

testChPow :: Bool
testChPow = chPow churchTwo churchThree == churchEight
  where
    churchTwo :: ChurchNumber
    churchTwo = chSucc (chSucc Zero)

    churchThree :: ChurchNumber
    churchThree = chAdd churchTwo (chSucc Zero)

    churchEight :: ChurchNumber
    churchEight = chAdd (chMult churchTwo churchTwo) (chMult churchTwo churchTwo)

testChPrev :: Bool
testChPrev = chPrev churchThree == churchTwo
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

    churchThree :: ChurchNumber
    churchThree = Succ (Succ (Succ Zero))


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
  putStrLn "testMyLength"
  print testMyLength

  putStrLn "testMyTail"
  print testMyTail

  putStrLn "testMyInit"
  print testMyInit

  putStrLn "testMyAppend"
  print testMyAppend

  putStrLn "testMyReverse"
  print testMyReverse

  putStrLn "testElemByIndex"
  print testElemByIndex

  putStrLn " "

  putStrLn "Task 6"

  putStrLn " "

  putStrLn "testChSucc"
  print testChSucc

  putStrLn "testChAdd"
  print testChAdd

  putStrLn "testChMult"
  print testChMult

  putStrLn "testChPow"
  print testChPow

  putStrLn "testChPrev"
  print testChPrev

  putStrLn " "

  putStrLn "Task 8"

  putStrLn " "

  putStrLn "testIsPresented1"
  print testIsPresented1

  putStrLn "testIsPresented2"
  print testIsPresented2





-------------------------------------------------------------------------------