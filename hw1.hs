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
myAnd = undefined

-------------------------------------------------------------------------------

-- 2. Реализуйте вычисление чисел Фибоначчи 2 способами.

-- | без хвостовой рекурсии
--
fib :: Int -> Int
fib = undefined

-- | c хвостовой рекурсией
--
fibTail :: Int -> Int
fibTail = undefined

-- Позапускайте обе функции на больших числах (> 1000). Какой результат вы получили и почему?

-------------------------------------------------------------------------------

-- 3. Повторяем каррирование. Реализуйте 2 функции.

-- | превращает некаррированную функцию в каррированную
--
myCurry :: ((a, b) -> c) -> a -> b -> c
myCurry = undefined

-- | превращает каррированную функцию в некаррированную
--
myUncurry :: (a -> b -> c) -> (a, b) -> c
myUncurry = undefined

-------------------------------------------------------------------------------

-- 4. Реализуйте функции для списка.
--    - используйте `Either` и `Maybe` для обработки ошибок в случаях, где это необходимо
--    - укажите типы функций
--    - напишите тесты

-- | возвращает длину списка
--
myLength = undefined

-- | возвращает хвост списка
--
myTail = undefined

-- | возвращает список без последнего элемента
--
myInit = undefined

-- | объединяет 2 списка
--
myAppend = undefined

-- | разворачивает список
--
myReverse = undefined

-- | выдаёт элемент списка по индексу
--
elemByIndex = undefined

-------------------------------------------------------------------------------

-- 5. Реализуйте map для разных типов.

mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d 
mapEither = undefined

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = undefined

mapList :: (a -> b) -> [a] -> [b]
mapList = undefined

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

chSucc = undefined

chAdd = undefined

chMult = undefined

chPow = undefined

chPrev = undefined

-- | Тесты могут выглядеть так
--
testChSucc :: Bool
testChSucc = chSucc churchTwo == churchThree
  where
    churchTwo :: ChurchNumber
    churchTwo = Succ (Succ Zero)

-------------------------------------------------------------------------------

-- 7. Создайте тип данных `Point` для 2D-точек. Используйте рекорды для именования полей (см. `Person` из практики).
--    Реализуйте функции для этого типа.

-- | двигает точку на заданное расстояние по каджой из координат
--
move = undefined

-- | возвращает дистанцию между 2 точками
--
dist = undefined

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
binTreeOfInts = undefined

-- Напишите поиск элемента в таком дереве.
--    - раскомментируйте строку с типом, когда объявите `IntTree`
--    - напишите тесты
--
-- isPresented :: Int -> IntTree -> Bool
isPresented = undefined

-------------------------------------------------------------------------------