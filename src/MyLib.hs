{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MyLib where

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.Foldable
import Data.List (sort)

-- Во всех заданиях с инстансами укажите сигнатуры функций

-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)

-- Hlint выдает "No hints", есть 1 ворнинг на 94 строке, я там его и объяснил

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
newtype Arrow a b = Arrow (a -> b)

-- Напишите инстанс Functor для Arrow и покажите выполнение законов

instance Functor (Arrow a) where
    fmap :: (b -> c) -> Arrow a b -> Arrow a c
    fmap f (Arrow g) = Arrow (f . g)

-- Не совсем понял, что значит покажите выполнение законов, поэтому прописал их в таком виде:

-- Закон fmap id = id
-- fmap id (Arrow g) = Arrow (id . g)  -- Определение fmap для Arrow
--                   = Arrow g         -- id . g = g
--                   = Arrow id        -- Определение id для Arrow
--                   = id (Arrow g)    -- Определение id

-- Закон fmap (f . g) = fmap f . fmap g
-- fmap (f . g) (Arrow h) = Arrow ((f . g) . h)  -- Определение fmap для Arrow
--                        = Arrow (f . (g . h))  -- Ассоциативность композиции функций
--                        = fmap f (Arrow (g . h))  -- Определение fmap для Arrow
--                        = fmap f (fmap g (Arrow h))  -- Определение fmap для Arrow



-------------------------------------------------------------------------------

-- 2. Студенты и Moinoid (1 балл)

-- | Тип данных "Студент"
--
data Student = Student 
    { name  :: String -- имя студента
    , grade :: Int    -- оценка студента по нашему предмету
    }

data StudentsLog = StudentsLog
    { studentNames :: [String]  -- список имён студентов
    , worstGrade   :: Maybe Int -- наименьшая оценка по курсу
    , bestGrade    :: Maybe Int -- наибольшая оценка по курсу
    }

-- 2.a Функция, которая по списку студентов курса рассчитывает информацию по курсу (0,5 балла)
--
calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog [] = StudentsLog [] Nothing Nothing
calculateStudentsLog students =
  StudentsLog
    { studentNames = map name students
    , worstGrade = if null students then Nothing else Just (minimum (map grade students))
    , bestGrade = if null students then Nothing else Just (maximum (map grade students))
    }

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--

instance Semigroup StudentsLog where
  (<>) :: StudentsLog -> StudentsLog -> StudentsLog
  (StudentsLog names1 worst1 best1) <> (StudentsLog names2 worst2 best2) =
    StudentsLog (names1 ++ names2)
                (minMaybe worst1 worst2)
                (maxMaybe best1 best2)

instance Monoid StudentsLog where
  mempty :: StudentsLog
  mempty = StudentsLog [] Nothing Nothing  -- нейтральный элемент

-- Вспомогательная функция для вычисления минимума, учитывая Nothing
minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing x = x
minMaybe x Nothing = x
minMaybe (Just a) (Just b) = Just (min a b)

-- Вспомогательная функция для вычисления максимума, учитывая Nothing
maxMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing x = x
maxMaybe x Nothing = x
maxMaybe (Just a) (Just b) = Just (max a b)

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' students = mconcat (map studentToLog students)
  where
    studentToLog :: Student -> StudentsLog
    studentToLog student = StudentsLog [name student] (Just (grade student)) (Just (grade student))

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node a children) = f a <> foldMap (foldMap f) children

-- Сделайте 'Tree' представителем класса типов 'Foldable'

-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1 балл)

data Apple = Apple
    { color  :: String -- цвет яблока
    , weight :: Float  -- вес яблока
    }
  deriving (Eq, Show, Ord)

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится 
--     в заданном диапазоне весов (0,25 балла)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree weightRange = getAll (foldMap checkWeight tree)
  where
    (minWeight, maxWeight) = weightRange
    checkWeight apple = All (weight apple >= minWeight && weight apple <= maxWeight)

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--

heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple tree = maximumByWeight $ foldMap (: []) tree
  where
    maximumByWeight [] = Nothing
    maximumByWeight appless = Just $ maximumBy (\a b -> compare (weight a) (weight b)) appless

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--

thisApple :: Tree Apple -> [String] -> (Float, Float) -> Maybe Apple
thisApple tree colors weightRange =
    find (\apple -> isColorMatch apple && isWeightInRange apple) (foldMap (: []) tree)
  where
    isColorMatch apple = color apple `elem` colors
    isWeightInRange apple = weight apple >= lowerBound && weight apple <= upperBound

    lowerBound = fst weightRange
    upperBound = snd weightRange

-- 4.d Считает сумму весов всех яблок в дереве (0,25 балла)
--

sumOfApples :: Tree Apple -> Float
sumOfApples tree = getSum $ foldMap (Sum . weight) tree

-------------------------------------------------------------------------------

-- 5. Корзинка с яблоками и Foldable (0,5 балла)

-- | Яблоки в корзинке расфасованы по цветам.
-- | Для каждого цвета яблоки упорядочены по весу
--
newtype Basket = Basket { apples :: M.Map String [Apple] }
  deriving (Eq, Show)

-- Реализуйте с помощью свёртки дерева функцию, которая соберёт 
-- по дереву яблок корзинку с яблоками.
-- В 'Data.Map.Strict' вы найдёте функции, которые помогут вам
-- инициализировать и модифицировать мапу
--      
collectBasket :: Tree Apple -> Basket
collectBasket tree = foldr collectApple emptyBasket (flattenTree tree) -- foldr для сохранения порядка яблок как они встречаются
  where
    emptyBasket = Basket M.empty -- пустая корзина

    -- функция для добавления яблока в корзину
    collectApple :: Apple -> Basket -> Basket
    collectApple apple (Basket basket) = Basket updatedBasket
      where
        updatedBasket = M.insertWith updateColor (color apple) [apple] basket

    -- функция для обновления весов 
    updateColor :: [Apple] -> [Apple] -> [Apple]
    updateColor newApples existingApples = sort (existingApples ++ newApples)

-- функция для выравнивания дерева в список яблок
flattenTree :: Tree Apple -> [Apple]
flattenTree Leaf = []
flattenTree (Node apple children) = apple : concatMap flattenTree children

-------------------------------------------------------------------------------

-- 6. Двоичная куча и Foldable (1,5 балла)
--    https://neerc.ifmo.ru/wiki/index.php?title=Двоичная_куча
--
data BinaryHeap a 
  = BinNode 
      { val   :: a
      , left  :: BinaryHeap a
      , right :: BinaryHeap a
      } 
  | BinLeaf
  deriving (Eq, Show)

-- 6.a Реализуйте функцию siftDown, восстанавливающую свойство кучи в куче (0,5 балла)
--      
siftDown :: Ord a => BinaryHeap a -> BinaryHeap a
siftDown BinLeaf = BinLeaf  -- в листе и делать нечего
siftDown (BinNode x BinLeaf BinLeaf) = BinNode x BinLeaf BinLeaf -- предлистовое состояние, все ок
siftDown (BinNode x leftt BinLeaf) -- справа лист, слева что-то есть
                               | x < val leftt = BinNode x leftt BinLeaf  -- Порядок не нарушен
                               | otherwise = BinNode (val leftt) (siftDown (leftt {val = x})) BinLeaf  -- Порядок нарушен
siftDown (BinNode x lleft rright)  -- классическая нода, слева справа что-то есть
                               | x < min (val lleft) (val rright) = BinNode x lleft rright   -- Порядок не нарушен
                               | val lleft < val rright = BinNode (val lleft) (siftDown (lleft {val = x})) rright  -- слева < справа
                               | otherwise = BinNode (val rright) lleft (siftDown (rright {val = x})) -- справа > слева


-- 6.b Реализуйте с помощью свёртки функцию buildHeap,
--     которая за __линейное время__ конструирует на основе спиcка элементов бинарную кучу.
--     Соответствующий алогритм описан в статье на вики (ссылка выше).
--     Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!)
--     (1 балл)
--       
-- buildHeap :: Ord a => [a] -> BinaryHeap a
-- buildHeap = foldr insertValue BinLeaf
--   where
--     insertValue :: Ord a => a -> BinaryHeap a -> BinaryHeap a
--     insertValue val BinLeaf = BinNode val BinLeaf BinLeaf
--     insertValue val (BinNode nodeVal left right)
--       | val <= nodeVal = BinNode val (insertValue nodeVal left) right
--       | otherwise      = BinNode nodeVal (insertValue val left) right

-- Не могу понять, почему эта функция строит косячную кучу, вроде бы делаю все правильно
-- По крайней мере я прописал тесты, как мне кажется, правильно, и они не выполняются :с
-- поэтому, пока не пойму в чем прикол, те тесты для buildHeap закомменчены 


-------------------------------------------------------------------------------

-- 7. A list with random access (2 балла)

-- Начнем с самой простой из всех структур данных - связанного списка.
-- Как вы хорошо знаете, поиск головной части списка происходит быстро, а произвольный доступ - гораздо медленнее:
-- xs !! n для получения n-го элемента списка требуется O(n), т.е. линейное время.
-- Мы хотели бы создать более быструю спископодобную структуру данных, которая сократила бы это время до O(log n)

-- | Зададим бинарное дерево, в листьях которого хранятся элементы a
-- | Кроме того, каждый узел в этом дереве аннотируется значением типа v (tag)
--
data BinaryTree v a = BLeaf v a | BBranch v (BinaryTree v a) (BinaryTree v a)
  deriving (Show)

-- Тогда наше дерево будет выглядеть так
--      v
--    /   \
--   v     v
--  / \   / \
-- v  v  v   v
-- a  a  a  / \
--         v   v
--         a   a

-- 7.a В листьях хранятся элементы нашего списка слева направо 
--     Реализуйте получение списка элементов из дерева (0,25 балла)
--
mytoList :: BinaryTree v a -> [a]
mytoList (BLeaf _ a) = [a] -- Если это лист, возвращаем его значение в виде списка
mytoList (BBranch _ leftt rightt) = mytoList leftt ++ mytoList rightt -- Рекурсивно обходим левое и правое поддерево и объединяем списки

-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)
--
tag :: BinaryTree v a -> v
tag (BLeaf tagValue _) = tagValue -- Если это лист, возвращаем его тег
tag (BBranch tagValue _ _) = tagValue -- Если это ветвь, возвращаем его тег

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
--
myhead :: BinaryTree v a -> a
myhead (BLeaf _ a) = a -- Если это лист, возвращаем его значение
myhead (BBranch _ leftt _) = myhead leftt -- Если это ветвь, переходим к левой ветви

-- Итак, доступ к первому листу был прост, а как быть со вторым, третьим, n-ым листом?
-- Решение состоит в том, чтобы аннотировать каждое поддерево его размером.

type Size = Int

-- Тогда наше дерево будет иметь тип `BinaryTree Size a` и выглядеть так
--      5
--    /   \
--   2     3
--  / \   / \
-- 1  1  1   2
-- a  a  a  / \
--         1   1
--         a   a

-- Таким образом, мы задаем v = Size и хотим, чтобы аннотации выполняли следующие условия
-- tag (BLeaf  ..)      = 1
-- tag (BBranch .. x y) = tag x + tag y

-- Мы можем убедиться в том, что они всегда корректны, используя свои "конструкторы":
-- вместо использования BLeaf и BBranch для создания дерева
leafSize :: a -> BinaryTree Size a
leafSize = BLeaf 1

branchSize :: BinaryTree Size a -> BinaryTree Size a -> BinaryTree Size a
branchSize x y = BBranch (tag x + tag y) x y

-- 7.d Создайте дерево типа `BinaryTree Size a`, используя leafSize и branchSize (0,25 балла)

-- Создаем листья с аннотациями размера 1
leaf1 :: BinaryTree Size Int
leaf2 :: BinaryTree Size Int
leaf3 :: BinaryTree Size Int
leaf4 :: BinaryTree Size Int
leaf5 :: BinaryTree Size Int
leaf1 = leafSize (1 :: Int)
leaf2 = leafSize (1 :: Int)
leaf3 = leafSize (1 :: Int)
leaf4 = leafSize (1 :: Int)
leaf5 = leafSize (1 :: Int)

-- Создаем ветви, комбинируя листья и аннотируя их размер
branch1 :: BinaryTree Size Int
branch2 :: BinaryTree Size Int
branch3 :: BinaryTree Size Int
branch4 :: BinaryTree Size Int
branch1 = branchSize leaf1 leaf2
branch2 = branchSize leaf4 leaf5
branch3 = branchSize leaf3 branch2
branch4 = branchSize branch1 branch3

-- Итоговое дерево с аннотацией размера
annotatedTree :: BinaryTree Size Int
annotatedTree = branch4

-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ a) 1 = a
getInd (BLeaf _ _) _ = error "Wrong indx"
getInd (BBranch _ leftt rightt) n
  | (n < 1) || (n > sizeOfLeft + 1) = error "Wrong indx"
  | n <= sizeOfLeft = getInd leftt n
  | otherwise = getInd rightt (n - sizeOfLeft)
  where
    sizeOfLeft = tag leftt


-------------------------------------------------------------------------------

-- 8. A priority queue (1,5 балл)

-- Рассмотрим другую структуру данных -- очередь с приоритетом.
-- Мы представляем приоритеты в виде целых чисел и представляем их как точки во времени,
-- поэтому наименьшие из них являются более приоритетными.

type Priority = Int

-- И снова мы используем бинарное дерево. На этот раз мы представляем его как турнирное дерево,
-- так что каждое поддерево аннотируется наименьшим приоритетом, который оно содержит

-- Тогда наше дерево будет иметь тип `BinaryTree Priority a` и выглядеть так
--      2
--    /   \
--   4     2
--  / \   / \
-- 16  4  2  8
-- a   a  a / \
--         32  8
--         a   a

-- Таким образом, мы задаем v = Priority и хотим, чтобы аннотации выполняли следующие условия
-- tag (Leaf .. a)     = priority a
-- tag (Branch .. x y) = tag x `min` tag y

-- 8.a Задайте собственные конструкторы leafPrio и branchPrio (на замену BLeaf и BBranch), чтобы
--     быть уверенными в корректности аннотаций по аналогии с leafSize и branchSize (0,25 балла)

data PriorTree a = PrLeaf Priority a | PrBranch Priority (PriorTree a) (PriorTree a)
  deriving (Show)

priority :: PriorTree a -> Priority
priority (PrLeaf v _) = v
priority (PrBranch v _ _) = v

leafPrior :: Priority -> a -> PriorTree a
leafPrior = PrLeaf

branchPrior :: PriorTree a -> PriorTree a -> PriorTree a
branchPrior a b = PrBranch (priority a `min` priority b) a b

-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)

prioTree :: PriorTree Char
prioTree = branchPrior (branchPrior (leafPrior 16 'a') (leafPrior 4 'a')) (branchPrior (leafPrior 2 'a') (branchPrior (leafPrior 32 'a') (leafPrior 8 'a')))

-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: PriorTree a -> a
getWinner (PrLeaf _ a) = a
getWinner (PrBranch _ leftt rightt) = case compare (priority leftt) (priority rightt) of
    LT -> getWinner leftt
    _  -> getWinner rightt

-------------------------------------------------------------------------------

-- 10. BinaryTree и Monoid (1 балла)

-- Как видим, одну и ту же древовидную структуру можно использовать для двух совершенно разных целей,
-- просто применяя разные аннотации. И распознав в аннотациях моноид, мы можем полностью унифицировать обе реализации.
-- Более того, операции getInd и getWinner -- это фактически частные случаи одной и той же функции! Какой?

-- Функции getInd и getWinner фактически представляют обобщенный поиск элемента в структуре данных с аннотациями. 
-- Эти функции можно обобщить, создав функцию get, которая принимает структуру, правило выбора элемента (например, функцию сравнения) и возвращает выбранный элемент.

-- Вспомним:
-- """
-- Таким образом, мы задаем v = Size и хотим, чтобы аннотации выполняли следующие условия
-- tag (BBranch .. x y) = tag x + tag y
-- """
-- """
-- Таким образом, мы задаем v = Priority и хотим, чтобы аннотации выполняли следующие условия
-- tag (Branch .. x y) = tag x `min` tag y
-- """

-- 10.a Обобщите эту функцию, написав инстансы `Monoid Size` и `Monoid Priority` (0,5 балла)
-- Kакую ошибку вы получили с при текущем определении Size и Priority, попытавшись создать инстансы?

-- Conflicting family instance declarations, т.к. хаскель не умеет в синонимы типов, если они идентичны уже существующим

-- Что нужно изменить в определении Size и Priority?

-- Создать их по новой с немного другим именем

newtype Size' = Size' Int 
  deriving (Eq, Show)

instance Semigroup Size' where
  (<>) :: Size' -> Size' -> Size'
  Size' x <> Size' y = Size' (x + y)

instance Monoid Size' where
  mempty :: Size'
  mempty = Size' 1

-- Законы для Monoid

-- Ассоциативность
-- (Size' x `mappend` Size' y) `mappend` Size' z = Size' x `mappend` (Size' y `mappend` Size' z)

-- Наличие нейтрального элемента
-- Size' 1 `mappend` Size' x = Size' x
-- Size' x `mappend` Size' 1 = Size' x

newtype Priority' = Priority' Int 
  deriving (Eq, Show)

instance Semigroup Priority' where
  (<>) :: Priority' -> Priority' -> Priority'
  Priority' x <> Priority' y = Priority' (min x y)

instance Monoid Priority' where
  mempty :: Priority'
  mempty = Priority' maxBound

-- | Теперь branchSize и branchPrio могут быть заменены на branch
--
branch :: Monoid v => BinaryTree v a -> BinaryTree v a -> BinaryTree v a
branch x y = BBranch (tag x <> tag y) x y

-- | Однако, мы не можем сделать то же с leaf. Почему?
--   Потому что у нас получились для него разные типы данных (размер и приоритет)

-- leaf :: Monoid v => a -> BinaryTree v a
-- leaf = BLeaf mempty

-- Чтобы задать leaf унифицированным способом аналогично branch, давайте создадим класс типов Measured

class Monoid v => Measured v a where
    measure :: a -> v

-- | Написав различные инстансы этого класса для наших `BinaryTree Size a` и `BinaryTree Priority a`,
-- | мы сможем по-разному вычислять аннотацию листа по значению. Тогда
-- 
leaf :: Measured v a => a -> BinaryTree v a
leaf x = BLeaf (measure x) x

-- 10.b Напишите инстансы Measured для Size и Priority (0,5 балла)

instance Measured Size' a where
  measure :: a -> Size'
  measure _ = Size' 1

instance Measured Priority' a where
  measure :: a -> Priority'
  measure _ = Priority' maxBound

-------------------------------------------------------------------------------
