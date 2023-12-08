{-# LANGUAGE InstanceSigs #-}

module MyLib where

import Data.Array qualified as A
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Data.Monoid

-- Во всех заданиях с инстансами укажите сигнатуры функций

-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
-- :p
--  Я нормально понял, что такое функторы, но вообще не понял, что здесь происходит (именно что такое стрелочка).
-- Где почитать можно? На хугле не нашел.
newtype Arrow a b = Arrow {getArrow :: a -> b}

instance Functor (Arrow a) where
  fmap :: (b -> c) -> Arrow a b -> Arrow a c
  fmap f (Arrow g) = Arrow $ f . g

-- Разобрался в том, как это ты доказывала.
-- Identity
-- fmap id == id
-- fmap id (Arrow f) = Arrow $ id . f = Arrow f
-- id (Arrow f) = Arrow f

-- Composition
-- fmap (f . g) == fmap f . fmap g
-- fmap (f . g) (Arrow h) = Arrow $ f . g . h
-- fmap g (Arrow h) = Arrow $ g . h
-- fmap f (Arrow $ g . h) = Arrow $ f . g . h
-------------------------------------------------------------------------------

-- 2. Студенты и Moinoid (1 балл)

-- | Тип данных "Студент"
data Student = Student
  { name :: String, -- имя студента
    grade :: Int -- оценка студента по нашему предмету
  }

data StudentsLog = StudentsLog
  { studentNames :: [String], -- список имён студентов
    worstGrade :: Maybe Int, -- наименьшая оценка по курсу
    bestGrade :: Maybe Int -- наибольшая оценка по курсу
  }
  deriving (Show)

-- 2.a Функция, которая по списку студентов курса рассчитывает информацию по курсу (0,5 балла)

calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog = foldr helper mempty
  where
    helper Student {name = name1, grade = grade1} StudentsLog {studentNames = names1, worstGrade = Just worstGrade1, bestGrade = Just bestGrade1} = StudentsLog {studentNames = name1 : names1, worstGrade = Just (min grade1 worstGrade1), bestGrade = Just (max grade1 bestGrade1)}
    helper Student {name = name1, grade = grade1} _ = StudentsLog {studentNames = [name1], worstGrade = Just grade1, bestGrade = Just grade1}

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)

(<==>) :: StudentsLog -> StudentsLog -> Bool
(<==>) log1 log2 = worstGrade log1 == worstGrade log2 && bestGrade log1 == bestGrade log2 && studentNames log1 == studentNames log2

instance Semigroup StudentsLog where
  (<>) :: StudentsLog -> StudentsLog -> StudentsLog
  (<>) log1 log2 = StudentsLog {studentNames = studentNames log1 <> studentNames log2, worstGrade = if isNothing (worstGrade log1) || isNothing (worstGrade log2) then max (worstGrade log1) (worstGrade log2) else min (worstGrade log1) (worstGrade log2), bestGrade = max (bestGrade log1) (bestGrade log2)}

instance Monoid StudentsLog where
  mempty :: StudentsLog
  mempty = StudentsLog {studentNames = [] :: [String], worstGrade = Nothing, bestGrade = Nothing}

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' = foldr helper mempty
  where
    helper Student {name = name1, grade = grade1} acc = StudentsLog {studentNames = [name1], worstGrade = Just grade1, bestGrade = Just grade1} <> acc

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)
-- Сделайте 'Tree' представителем класса типов 'Foldable'

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

-- instance (Semigroup a) => Semigroup (Tree a) where
--   (<>) :: Tree a -> Tree a -> Tree a
--   (<>) (Node value0 children0) (Node value1 children1) = Node (value0 <> value1) (children0 <> children1)

-- instance (Monoid a) => Monoid (Tree a) where
--   mempty = Leaf

-- myFirstTree :: Tree Int
-- myFirstTree = Node 0 [Node 1 [Node 2 [Leaf]]]

-- а каким тестом проверить, что это работает?
-- :p
instance Foldable Tree where
  foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node value children) = mconcat $ f value : map (foldMap f) children

-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1 балл)

data Apple = Apple
  { color :: String, -- цвет яблока
    weight :: Float -- вес яблока
  }
  deriving (Eq, Show)

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится
--     в заданном диапазоне весов (0,25 балла)
--

instance Ord Apple where
  (<=) :: Apple -> Apple -> Bool
  (<=) apple1 apple2 = weight apple1 <= weight apple2

(<!=>) :: Apple -> Apple -> Bool
(<!=>) apple1 apple2 = color apple1 == color apple2 && weight apple1 == weight apple2


applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange Leaf (_, _) = True
applesInRange tree (min', max') = weight (minimum tree) >= min' && weight (maximum tree) <= max'

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple Leaf = Nothing
heaviestApple tree = Just $ maximum tree

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--
-- :p, но разобрался
thisApple :: Tree Apple -> [String] -> (Int, Int) -> Maybe Apple
thisApple Leaf _ _ = Nothing
thisApple tree colors (l, r) = getFirst $ foldMap (First . f) tree
  where
    f :: Apple -> Maybe Apple
    f apple =
      if (weight apple <= fromIntegral r) && (weight apple >= fromIntegral l) && elem (color apple) colors
        then Just apple
        else Nothing

-- 4.d Считает сумму весов всех яблок в дереве (0,25 балла)

sumOfApples :: Tree Apple -> Float
sumOfApples tree = getSum $ foldMap (Sum . weight) tree

-------------------------------------------------------------------------------

-- 5. Корзинка с яблоками и Foldable (0,5 балла)

-- | Яблоки в корзинке расфасованы по цветам.
-- | Для каждого цвета яблоки упорядочены по весу
newtype Basket = Basket {apples :: M.Map String [Apple]}
  deriving (Eq, Show)

-- Реализуйте с помощью свёртки дерева функцию, которая соберёт
-- по дереву яблок корзинку с яблоками.
-- В 'Data.Map.Strict' вы найдёте функции, которые помогут вам
-- инициализировать и модифицировать мапу

collectBasket :: Tree Apple -> Basket
collectBasket Leaf = Basket M.empty
collectBasket tree = Basket . M.fromListWith (<>) . foldMap (\x -> [(color x, [x])]) $ tree

-------------------------------------------------------------------------------

-- 6. Двоичная куча и Foldable (1,5 балла)
--    https://neerc.ifmo.ru/wiki/index.php?title=Двоичная_куча
--
data BinaryHeap a
  = BinNode
      { val :: a,
        left :: BinaryHeap a,
        right :: BinaryHeap a
      }
  | BinLeaf
  deriving (Eq, Show)

-- 6.a Реализуйте функцию siftDown, восстанавливающую свойство кучи в куче (0,5 балла)
-- это твоя реализация, круто, что у тебя получилось! Вроде бы все понял.
siftDown :: (Ord a) => BinaryHeap a -> BinaryHeap a
siftDown heap
  | heap == BinLeaf = heap
  | left heap /= BinLeaf && (val . left $ heap) < val heap = processLeft (val heap) (left heap) (right heap)
  | right heap /= BinLeaf && (val . right $ heap) < val heap = processRight (val heap) (left heap) (right heap)
  | otherwise = heap
  where
    processLeft v l = BinNode (val l) (siftDown BinNode {val = v, left = left l, right = right l})
    processRight v l r = BinNode (val r) l (siftDown BinNode {val = v, left = left r, right = right r})

-- 6.b Реализуйте с помощью свёртки функцию buildHeap,
--     которая за __линейное время__ конструирует на основе спиcка элементов бинарную кучу.
--     Соответствующий алогритм описан в статье на вики (ссылка выше).
--     Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!)
--     (1 балл)
--
-- ｀、ヽ｀ヽ｀、ヽ(ノ＞＜)ノ ｀、ヽ｀☂ヽ｀、ヽ
buildHeap :: (Ord a) => [a] -> BinaryHeap a
buildHeap arr = foldl insert emptyArr (reverse $ zip [1 ..] arr) A.! 1
  where
    len :: Int
    len = length arr

    emptyArr :: A.Array Int (BinaryHeap a)
    emptyArr = A.listArray (1, len) (replicate len BinLeaf)

    insert :: (Ord a) => A.Array Int (BinaryHeap a) -> (Int, a) -> A.Array Int (BinaryHeap a)
    insert acc (i, x) = acc A.// [(i, siftDown $ BinNode x (getChild (2 * i) acc) (getChild (2 * i + 1) acc))]

    getChild :: Int -> A.Array Int (BinaryHeap a) -> BinaryHeap a
    getChild i acc = if i <= len then acc A.! i else BinLeaf

-- [a] A.! i -> gets the value at index i
-------------------------------------------------------------------------------

-- 7. A list with random access (2 балла)

-- Начнем с самой простой из всех структур данных - связанного списка.
-- Как вы хорошо знаете, поиск головной части списка происходит быстро, а произвольный доступ - гораздо медленнее:
-- xs !! n для получения n-го элемента списка требуется O(n), т.е. линейное время.
-- Мы хотели бы создать более быструю спископодобную структуру данных, которая сократила бы это время до O(log n)

-- | Зададим бинарное дерево, в листьях которого хранятся элементы a
-- | Кроме того, каждый узел в этом дереве аннотируется значением типа v (tag)
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

toList :: BinaryTree v a -> [a]
toList (BLeaf _ a) = [a]
toList (BBranch _ left1 right1) = toList left1 ++ toList right1

-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)

tag :: BinaryTree v a -> v
tag (BLeaf v _) = v
tag (BBranch v _ _) = v

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
--
head' :: BinaryTree v a -> a
head' (BLeaf _ a) = a
head' (BBranch _ left1 _) = head' left1

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
myTree :: BinaryTree Size Int
myTree = branchSize (branchSize (branchSize (leafSize 1) (leafSize 2)) (leafSize 3)) (branchSize (leafSize 4) (leafSize 5))

-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd = helper
  where
    helper tree n | tag tree < n = error "Unreachable id"
    helper (BLeaf _ a) _ = a
    helper (BBranch _ left_ right_) n = if tag left_ >= n then helper left_ n else helper right_ (n - tag left_)

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
branchPrio tree1 tree2 = BBranch (tag tree1 `min` tag tree2) tree1 tree2

-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)
myPriorityTree :: BinaryTree Priority Int
myPriorityTree = branchPrio (branchPrio (leafPrio 16 0) (leafPrio 4 1)) (branchPrio (leafPrio 2 (-1)) (branchPrio (leafPrio 32 3) (leafPrio 8 4)))

--      2
--    /   \
--   4     2
--  / \   / \
-- 16  4  2  8
-- a   a  a / \
--         32  8
--         a   a

-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: BinaryTree Priority a -> a
getWinner (BLeaf _ el) = el
getWinner (BBranch _ left_ right_) = if tag left_ <= tag right_ then getWinner left_ else getWinner right_

-------------------------------------------------------------------------------

-- сложна
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
-- Kакую ошибку вы получили с при текущем определении Size и Priority, попытавшись создать инстансы?
-- Что нужно изменить в определении Size и Priority?

-- | Теперь branchSize и branchPrio могут быть заменены на branch
-- branch :: (Monoid v) => BinaryTree v a -> BinaryTree v a -> BinaryTree v a
-- branch x y = BBranch (tag x <> tag y) x y

-- | Однако, мы не можем сделать то же с leaf. Почему?
--
-- leaf :: Monoid v => a -> BinaryTree v a
-- leaf = BLeaf mempty

-- Чтобы задать leaf унифицированным способом аналогично branch, давайте создадим класс типов Measured

-- class (Monoid v) => Measured v a where
--   measure :: a -> v

-- -- | Написав различные инстансы этого класса для наших `BinaryTree Size a` и `BinaryTree Priority a`,
-- -- | мы сможем по-разному вычислять аннотацию листа по значению. Тогда
-- leaf :: (Measured v a) => a -> BinaryTree v a
-- leaf x = BLeaf (measure x) x

-- 10.b Напишите инстансы Measured для Size и Priority (0,5 балла)

-------------------------------------------------------------------------------
