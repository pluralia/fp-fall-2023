{-# LANGUAGE InstanceSigs, MultiParamTypeClasses, FlexibleInstances #-}

module MyLib where

import           Data.List (foldl')
import qualified Data.Map.Strict as M

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

-- Законы:
-- fmap id (Arrow g) = Arrow (id . g) = Arrow g
-- fmap (f . g) (Arrow h) = Arrow ((f . g) . h) = Arrow (f . (g . h)) = fmap f (fmap g (Arrow h))
-------------------------------------------------------------------------------

-- 2. Студенты и Moinoid (1 балл)

-- | Тип данных "Студент"
--
data Student = Student 
    { name  :: String -- имя студента
    , grade :: Int    -- оценка студента по нашему предмету
    }deriving (Eq, Show)

data StudentsLog = StudentsLog
    { studentNames :: [String]  -- список имён студентов
    , worstGrade   :: Maybe Int -- наименьшая оценка по курсу
    , bestGrade    :: Maybe Int -- наибольшая оценка по курсу
    }deriving (Eq, Show)

-- 2.a Функция, которая по списку студентов курса рассчитывает информацию по курсу (0,5 балла)
--
calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog = foldl' helper (StudentsLog [] (Just maxBound) (Just minBound))
  where
    helper :: StudentsLog -> Student -> StudentsLog
    helper (StudentsLog names worst best) (Student n g) = 
      StudentsLog (n:names) (min <$> worst <*> Just g) (max <$> best <*> Just g)

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--

instance Semigroup StudentsLog where
  (<>) :: StudentsLog -> StudentsLog -> StudentsLog
  (<>) (StudentsLog names1 worst1 best1) (StudentsLog names2 worst2 best2) = 
    StudentsLog (names1 ++ names2) (min <$> worst1 <*> worst2) (max <$> best1 <*> best2)


instance Monoid StudentsLog where
  mempty :: StudentsLog
  mempty = StudentsLog [] (Just maxBound) (Just minBound)

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' = foldMap (\(Student n g) -> StudentsLog [n] (Just g) (Just g))

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

-- Сделайте 'Tree' представителем класса типов 'Foldable'

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr f acc (Node a children) = f a $ foldr (flip (foldr f)) acc children 

-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1 балл)

data Apple = Apple
  { color  :: String -- цвет яблока
  , weight :: Float  -- вес яблока
  }
  deriving (Eq, Show)

appleTree :: Tree Apple
appleTree = Node (Apple "red" 0.3) [Node (Apple "green" 0.2) [Leaf], Node (Apple "red" 0.1) [Leaf]]

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится 
--     в заданном диапазоне весов (0,25 балла)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree (minim, maxim) = all (\(Apple _ w) -> w >= minim && w <= maxim) tree

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple = foldr helper Nothing
  where
    helper :: Apple -> Maybe Apple -> Maybe Apple
    helper a Nothing = Just a
    helper a (Just b) = if weight a > weight b then Just a else Just b

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--
thisApple :: Tree Apple -> [String] -> (Float, Float) -> Maybe Apple
thisApple tree colors (minim, maxim) = foldr helper Nothing tree
  where
    helper :: Apple -> Maybe Apple -> Maybe Apple
    helper a Nothing = if color a `elem` colors && weight a >= minim && weight a <= maxim 
                       then Just a 
                       else Nothing
    helper _ (Just b) = Just b

-- 4.d Считает сумму весов всех яблок в дереве (0,25 балла)
--
sumOfApples :: Tree Apple -> Float
sumOfApples = sum . foldMap (\(Apple _ w) -> [w])

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
collectBasket = foldr helper (Basket M.empty)
  where
    helper :: Apple -> Basket -> Basket
    helper a (Basket m) = Basket $ M.insertWith merge (color a) [a] m

    merge :: [Apple] -> [Apple] -> [Apple]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys) = if weight x < weight y 
                          then x : merge xs (y:ys) 
                          else y : merge (x:xs) ys


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


binHeap :: BinaryHeap Int
binHeap = BinNode 3 (BinNode 2 BinLeaf BinLeaf) (BinNode 4 BinLeaf BinLeaf)
        
-- 6.a Реализуйте функцию siftDown, восстанавливающую свойство кучи в куче (0,5 балла)
--     
siftDown :: Ord a => BinaryHeap a -> Maybe (BinaryHeap a)
siftDown BinLeaf = Just BinLeaf
siftDown (BinNode v BinLeaf BinLeaf) = Just $ BinNode v BinLeaf BinLeaf
siftDown (BinNode v l BinLeaf) | v < val l = Just $ BinNode v l BinLeaf
                               | otherwise = Just $ BinNode (val l) ((unpack . siftDown) (l {val = v})) BinLeaf
siftDown (BinNode _ BinLeaf _) = Nothing
siftDown (BinNode v l r)       | v < min (val l) (val r) = Just $ BinNode v l r
                               | v > val l = Just $ BinNode (val l) ((unpack . siftDown) (l {val = v})) r
                               | otherwise = Just $ BinNode (val r) l ((unpack . siftDown) (r {val = v}))

unpack:: Maybe (BinaryHeap a) -> BinaryHeap a
unpack (Just x) = x
unpack Nothing = error "siftDown returned Nothing"

-- 6.b Реализуйте с помощью свёртки функцию buildHeap,
--     которая за __линейное время__ конструирует на основе спиcка элементов бинарную кучу.
--     Соответствующий алогритм описан в статье на вики (ссылка выше).
--     Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!)
--     (1 балл)
--       
buildHeap :: Ord a => [a] -> BinaryHeap a
buildHeap = foldl' helper BinLeaf
  where 
    helper :: Ord a => BinaryHeap a -> a -> BinaryHeap a
    helper BinLeaf x = BinNode x BinLeaf BinLeaf
    helper (BinNode y l r) x
                | x <= y    = BinNode x (helper r y) l
                | otherwise = BinNode y (helper r x) l
-- Этот алгоритм работает за O(n log n). Я не понял как сделать за O(n)
  
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
--
head' :: BinaryTree v a -> a
head' (BLeaf _ a) = a
head' (BBranch _ l _) = head' l

-- Итак, доступ к первому листу был прост, а как быть со вторым, третьим, n-ым листом?
-- Решение состоит в том, чтобы аннотировать каждое поддерево его размером.

type Size = Int

-- Тогда наше дерево будет иметь тип `BinaryTree Size a` и выглядеть так
--      5
--    /   \
--   2     3
--  / \   / \
-- 1  1  1   2
-- a  b  c  / \
--         1   1
--         d   e
binaryTree :: BinaryTree Int Char
binaryTree = BBranch 5 (BBranch 2 (BLeaf 1 'a') (BLeaf 1 'b')) (BBranch 3 (BLeaf 1 'c') (BBranch 2 (BLeaf 1 'd') (BLeaf 1 'e')))

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

binTree :: BinaryTree Size Char
binTree = branchSize (branchSize (leafSize 'a') (leafSize 'b')) (branchSize (leafSize 'c') (branchSize (leafSize 'd') (leafSize 'e')))

-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ a) 0 = a
getInd (BLeaf _ _) _ = error "Index out of range"
getInd (BBranch _ l r) n | n < tag l = getInd l n
                         | otherwise = getInd r (n - tag l)


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
-- a   b  c / \
--         32  8
--         d   e

-- Таким образом, мы задаем v = Priority и хотим, чтобы аннотации выполняли следующие условия
-- tag (Leaf .. a)     = priority a
-- tag (Branch .. x y) = tag x `min` tag y

-- 8.a Задайте собственные конструкторы leafPrio и branchPrio (на замену BLeaf и BBranch), чтобы
--     быть уверенными в корректности аннотаций по аналогии с leafSize и branchSize (0,25 балла)

data PrioTree a = PLeaf Priority a | PBranch Priority (PrioTree a) (PrioTree a)
  deriving (Show)

prio :: PrioTree a -> Priority
prio (PLeaf p _) = p
prio (PBranch p _ _) = p

leafPrio :: Priority -> a -> PrioTree a
leafPrio = PLeaf

branchPrio :: PrioTree a -> PrioTree a -> PrioTree a
branchPrio x y = PBranch (prio x `min` prio y) x y

-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)

prioTree :: PrioTree Char
prioTree = branchPrio (branchPrio (leafPrio 16 'a') (leafPrio 4 'b')) (branchPrio (leafPrio 2 'c') (branchPrio (leafPrio 32 'd') (leafPrio 8 'e')))

-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: PrioTree a -> a
getWinner (PLeaf _ a) = a
getWinner (PBranch _ l r) | prio l < prio r = getWinner l
                          | otherwise       = getWinner r

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
-- Kакую ошибку вы получили с при текущем определении Size и Priority, попытавшись создать инстансы?

-- Duplicate instance declarations. Что-то типо такого причина: Проблема здесь в том, что Priority и Size 
-- являются синонимами типов для Int, а не новыми типами. Haskell не позволяет создавать новые экземпляры 
-- синонимов типов, идентичных существующим экземплярам.

-- Что нужно изменить в определении Size и Priority?

-- Создать новый тип данных, а не синоним типа данных

newtype Size' = Size' Int deriving (Eq, Show)

instance Semigroup Size' where
  (<>) :: Size' -> Size' -> Size'
  Size' x <> Size' y = Size' (x + y)

instance Monoid Size' where
  mempty :: Size'
  mempty = Size' 0 -- чтобы законы выполнялись

-- Законы:
-- mempty <> mempty = mempty
-- mempty <> x = x
-- x <> mempty = x
-- (x <> y) <> z = x <> (y <> z)


newtype Priority' = Priority' Int deriving (Eq, Show)

instance Semigroup Priority' where
  (<>) :: Priority' -> Priority' -> Priority'
  Priority' x <> Priority' y = Priority' (min x y)

instance Monoid Priority' where
  mempty :: Priority'
  mempty = Priority' maxBound -- чтобы законы выполнялись

-- Законы:
-- mempty <> mempty = mempty
-- mempty <> x = x
-- x <> mempty = x
-- (x <> y) <> z = x <> (y <> z)

-- | Теперь branchSize и branchPrio могут быть заменены на branch
--
branch :: Monoid v => BinaryTree v a -> BinaryTree v a -> BinaryTree v a
branch x y = BBranch (tag x <> tag y) x y

-- | Однако, мы не можем сделать то же с leaf. Почему?
--
-- leaf :: Monoid v => a -> BinaryTree v a
-- leaf = BLeaf mempty

-- Ответ: size у leaf == 0. Значит size у branch == 0. Но это не так.

newBinTree :: BinaryTree Size' Char
newBinTree = branch (branch (leaf 'a') (leaf 'b')) (branch (leaf 'c') (branch (leaf 'd') (leaf 'e')))


newPrioTree :: BinaryTree Priority' Char
newPrioTree = branch (branch (leaf 'a') (leaf 'b')) (branch (leaf 'c') (branch (leaf 'd') (leaf 'e')))


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

instance Enum a => Measured Priority' a where
  measure :: a -> Priority'
  measure = Priority' . fromEnum
-------------------------------------------------------------------------------


  
  
  
