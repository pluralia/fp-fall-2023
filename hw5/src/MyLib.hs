{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module MyLib where

import qualified Data.Map.Strict as M
import Data.Monoid
import Data.List (sortBy)
-- Во всех заданиях с инстансами укажите сигнатуры функций

-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
newtype Arrow a b = Arrow (a -> b)

-- Напишите инстанс Functor для Arrow и покажите выполнение законов
instance Functor (Arrow a) where
    fmap :: (b -> c) -> Arrow a b -> Arrow a c
    fmap f (Arrow g) = Arrow (f . g)

-- Закон идентичности
-- fmap id (Arrow g) = Arrow (id . g) = Arrow g  -- применяем fmap с тождественной функцией
                                                 -- поскольку id . g эквивалентно g
-- Закон композиции
-- fmap (f . g) (Arrow h) = Arrow ((f . g) . h) = Arrow (f . (g . h)) = fmap f (fmap g (Arrow h))
-- сначала применяем fmap с композицией функций
-- далее используем ассоциативность композиции функций
-- и наконец, разбиваем на два последовательных вызова fmap


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
    }deriving(Show, Eq)

-- 2.a Функция, которая по списку студентов курса рассчитывает информацию по курсу (0,5 балла)
--
calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog students = StudentsLog
    { studentNames = [name student | student <- students] 
    , worstGrade = minimumGrade students
    , bestGrade = maximumGrade students
    }
  where
    minimumGrade :: [Student] -> Maybe Int
    minimumGrade [] = Nothing
    minimumGrade studentList = Just (minimum [grade student | student <- studentList])

    maximumGrade :: [Student] -> Maybe Int
    maximumGrade [] = Nothing
    maximumGrade studentList = Just (maximum [grade student | student <- studentList])

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--

instance Semigroup StudentsLog where
    (<>) :: StudentsLog -> StudentsLog -> StudentsLog
    StudentsLog names1 worst1 best1 <> StudentsLog names2 worst2 best2 =
        StudentsLog (names1 ++ names2) (minMaybe worst1 worst2) (maxMaybe best1 best2)
      where
        minMaybe :: Maybe Int -> Maybe Int -> Maybe Int
        minMaybe (Just x) (Just y) = Just (min x y)
        minMaybe (Just x) Nothing = Just x
        minMaybe Nothing (Just y) = Just y
        minMaybe Nothing Nothing = Nothing
        
        maxMaybe :: Maybe Int -> Maybe Int -> Maybe Int
        maxMaybe (Just x) (Just y) = Just (max x y)
        maxMaybe (Just x) Nothing = Just x
        maxMaybe Nothing (Just y) = Just y
        maxMaybe Nothing Nothing = Nothing

-- Упрощенные версии функций minMaybe и maxMaybe
-- minMaybe :: Maybe Int -> Maybe Int -> Maybe Int
-- minMaybe x y = min <$> x <*> y
--
-- maxMaybe :: Maybe Int -> Maybe Int -> Maybe Int
-- maxMaybe x y = max <$> x <*> y

instance Monoid StudentsLog where
    mempty :: StudentsLog
    mempty = StudentsLog [] Nothing Nothing

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' students = mconcat [StudentsLog [name student] (Just (grade student)) (Just (grade student)) | student <- students]


-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

-- Сделайте 'Tree' представителем класса типов 'Foldable'

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf = mempty
    foldMap f (Node x children) = f x `mappend` foldMap (foldMap f) children
-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1 балл)

data Apple = Apple
    { color  :: String -- цвет яблока
    , weight :: Float  -- вес яблока
    }
  deriving (Eq, Show)

instance Semigroup Apple where
    (Apple c1 w1) <> (Apple c2 w2) = Apple (c1 ++ c2) (w1 + w2)

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится 
--     в заданном диапазоне весов (0,25 балла)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree (minWeight, maxWeight) = all (\apple -> minWeight <= weight apple && weight apple <= maxWeight) tree

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple = foldr (maxBy weight) Nothing
  where
    maxBy :: Ord b => (a -> b) -> a -> Maybe a -> Maybe a
    maxBy _ x Nothing = Just x
    maxBy f x (Just y) | f x >= f y = Just x
                       | otherwise   = Just y

-- тут не увидел как упростить реализацию с помощью maximumBy
-- кажется, можно придумать более элегантную реализацию, 
-- но отдам предпочтение в пользу более "понятной"

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--
thisApple :: Tree Apple -> [String] -> (Float, Float) -> Maybe Apple
thisApple tree colors (minWeight, maxWeight) =
  foldr (\apple acc -> if isDesiredApple apple then Just apple else acc) Nothing tree
  where
    isDesiredApple :: Apple -> Bool
    isDesiredApple (Apple appleColor appleWeight) =
      appleColor `elem` colors && appleWeight >= minWeight && appleWeight <= maxWeight

-- Заменил weightRange на (minWeight, maxWeight),
-- и затем эти значения используются непосредственно в функции isDesiredApple.


-- 4.d Считает сумму весов всех яблок в дереве (0,25 балла)
--
sumOfApples :: Tree Apple -> Float
sumOfApples = getSum . foldMap (Sum . weight)

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
collectBasket = Basket . M.map (sortBy (\x y -> compare (weight x) (weight y))) . foldr collectApple M.empty -- использую sortBy
  where
    collectApple :: Apple -> M.Map String [Apple] -> M.Map String [Apple]
    collectApple apple = M.insertWith (++) (color apple) [apple]

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
siftDown BinLeaf = BinLeaf
siftDown (BinNode v BinLeaf BinLeaf) = BinNode v BinLeaf BinLeaf
siftDown (BinNode v BinLeaf r) = if v < val r
                                   then BinNode v BinLeaf r
                                   else BinNode (val r) BinLeaf (siftDown (r {val = v}))
siftDown (BinNode v l BinLeaf) = if v < val l
                                   then BinNode v l BinLeaf
                                   else BinNode (val l) (siftDown (l {val = v})) BinLeaf
siftDown (BinNode v l r)
    | v < min (val l) (val r) = BinNode v l r
    | val l < val r = BinNode (val l) (siftDown (l {val = v})) r
    | otherwise = BinNode (val r) l (siftDown (r {val = v}))

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
  deriving (Show, Eq)

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
toList :: BinaryTree v a -> [a]
toList (BLeaf _ a) = [a]
toList (BBranch _ l r) = toList l ++ toList r


-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)

tag :: BinaryTree v a -> v
tag (BLeaf v _) = v
tag (BBranch v _ _) = v

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
-- переименовал функцию из-за совпадения названия с библиотечной
head' :: BinaryTree v a -> a
head' (BLeaf _ a) = a
head' (BBranch _ l _) = head' l  -- Рекурсивно идем влево

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
treeWithSizeAnnotations :: BinaryTree Size Char
treeWithSizeAnnotations =
  branchSize
    (branchSize
      (branchSize
        (leafSize 'a')
        (leafSize 'b')
      )
      (leafSize 'c')
    )
    (branchSize
      (leafSize 'd')
      (leafSize 'e')
    )

-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ a) 1 = a
getInd (BLeaf _ _) _ = error "Leaf index out of bounds"
getInd (BBranch _ l r) n
  | n <= leftSize = getInd l n
  | otherwise = getInd r (n - leftSize)
  where
    leftSize = tag l
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
leafPrio :: Priority -> a -> BinaryTree Priority a
leafPrio = BLeaf

branchPrio :: BinaryTree Priority a -> BinaryTree Priority a -> BinaryTree Priority a
branchPrio l r = BBranch (tag l `min` tag r) l r

-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)
treeWithPriorityAnnotations :: BinaryTree Priority Char
treeWithPriorityAnnotations =
  branchPrio
    (branchPrio
      (branchPrio
        (leafPrio 16 'a')
        (leafPrio 4 'b')
      )
      (leafPrio 2 'c')
    )
    (branchPrio
      (leafPrio 8 'd')
      (branchPrio
        (leafPrio 32 'e')
        (leafPrio 8 'f')
      )
    )

-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: BinaryTree Priority a -> a
getWinner (BLeaf _ a) = a            -- Если это лист, он является победителем
getWinner (BBranch _ l r)
  | tag l <= tag r = getWinner l     -- Если выполнено условие, то выбираем победителя из левого поддерева
  | otherwise = getWinner r          -- В противном случае выбираем победителя из правого поддерева


-------------------------------------------------------------------------------

-- 10. BinaryTree и Monoid (1 балла)

-- Как видим, одну и ту же древовидную структуру можно использовать для двух совершенно разных целей,
-- просто применяя разные аннотации. И распознав в аннотациях моноид, мы можем полностью унифицировать обе реализации.
-- Более того, операции getInd и getWinner -- это фактически частные случаи одной и той же функции! Какой?

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
-- Kакую ошибку вы получили при текущем определении Size и Priority, попытавшись создать инстансы?
-- Мы получим Duplicate instance declaration. Это связано с тем, что Size и Priority по сути являются представителями типа
-- Int. Поэтому Haskell не позволяет создать несколько идентичных экземпляров типов.

-- Что нужно изменить в определении Size и Priority?
-- Для исправления этого можно создать новый тип данных

-- Определение нового типа данных NewSize для представления размера (целое число)
newtype NewSize = NewSize{getNewSize :: Int}
 deriving (Eq, Show)

-- Реализация операции (<>) для типа NewSize, которая сложит два размера
instance Semigroup NewSize where
  (<>) a b = NewSize (getNewSize a + getNewSize b)

-- Определение начального элемента моноида (единичного элемента) для NewSize
instance Monoid NewSize where
  mempty = NewSize (0 :: Int)

-- здесь руководствовался тем, что для NewSize требуется определить (<>) через операцию сложения,
-- нейтральным элементом которой является 0

-- Определение нового типа данных NewPriority для представления приоритета (целое число)
newtype NewPriority = NewPriority{getNewPriority :: Int}
 deriving (Eq, Show)

-- Реализация операции (<>) для типа NewPriority, которая выберет минимальный приоритет
instance Semigroup NewPriority where
  (<>) a b = NewPriority $ min (getNewPriority a) (getNewPriority b)

-- Определение начального элемента моноида (единичного элемента) для NewPriority
instance Monoid NewPriority where
  mempty = NewPriority (maxBound :: Int)

-- | Теперь branchSize и branchPrio могут быть заменены на branch
--
branch :: Monoid v => BinaryTree v a -> BinaryTree v a -> BinaryTree v a
branch x y = BBranch (tag x <> tag y) x y

-- | Однако, мы не можем сделать то же с leaf. Почему?
--
-- В случае листа нам требуется задать приоритет для NewPriority,
-- но для NewSize размер задавать не нужно.
-- Значит имеем коллизию типов данных для Leaf.
--
-- Если представить, что мы не принимаем приоритет в качестве аргумента и выводим его из самого значения,
-- т.е. типы leaf для Size и Priority соответствуют друг другу. Мы все равно не сможем воспользоваться операцией Monoid,
-- потому что для этого нам нужно определить mempty, которое по сути является пустым множеством, но лист им не является.

-- Допустим, у нас есть синонимы типов для описания расстояния в метрах и футах:
-- type Meters = Double
-- type Feet = Double
--
-- Теперь, если мы хотим создать инстансы класса типов Show для Meters и Feet,
-- мы столкнемся с ошибкой "Duplicate instance declaration",
-- потому что Haskell рассматривает их как один и тот же тип (в данном случае Double),
-- даже если у нас есть разные синонимы для него.

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
instance Measured NewSize a where
  measure :: a -> NewSize
  measure _ = NewSize {getNewSize = 1 :: Int}

instance (Enum a) => Measured NewPriority a where
  measure :: (Enum a) => a -> NewPriority
  measure priority = NewPriority (fromEnum priority) -- тут берем в  качестве приоритета то, что приходит, а не maxBound
                                                     -- это отлично иллюстрирует тест branch l1 l3
-------------------------------------------------------------------------------
    

