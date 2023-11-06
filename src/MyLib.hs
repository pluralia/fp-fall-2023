{-# LANGUAGE InstanceSigs, FlexibleInstances, MultiParamTypeClasses #-}

module MyLib where

import qualified Data.Map.Strict as M
import qualified Data.Array      as A
import Data.Monoid
    ( First(First, getFirst), All(All, getAll), Sum(Sum, getSum) )
import           Data.Foldable        (foldl')
import           Data.List            (sort)
import Control.Applicative (Alternative((<|>)))

-- Во всех заданиях с инстансами укажите сигнатуры функций

-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
newtype Arrow a b = Arrow { getArrow :: a -> b }

instance Functor (Arrow a) where                -- аналогично Either - один тип надо зафиксировать
    fmap :: (b -> c) -> Arrow a b -> Arrow a c
    fmap f (Arrow g) = Arrow $ f . g

-- Напишите инстанс Functor для Arrow и покажите выполнение законов

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
--
data Student = Student
    { name  :: String -- имя студента
    , grade :: Int    -- оценка студента по нашему предмету
    }
    deriving (Show, Eq)

data StudentsLog = StudentsLog
    { studentNames :: [String]  -- список имён студентов
    , worstGrade   :: Maybe Int -- наименьшая оценка по курсу
    , bestGrade    :: Maybe Int -- наибольшая оценка по курсу
    }
    deriving (Show, Eq)

instance Semigroup StudentsLog where
    (<>) :: StudentsLog -> StudentsLog -> StudentsLog
    StudentsLog n1 w1 b1 <> StudentsLog n2 w2 b2 =
       StudentsLog { studentNames = n1 <> n2
                   , worstGrade   = min <$> w1 <*> w2 <|> w1 <|> w2
                   , bestGrade    = max <$> b1 <*> b2 <|> b1 <|> b2 }

instance Monoid StudentsLog where
    mempty :: StudentsLog
    mempty = StudentsLog [] Nothing Nothing

-- 2.a Функция, которая по списку студентов курса рассчитывает информацию по курсу (0,5 балла)
--
calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog list | null list = StudentsLog { studentNames = [] :: [String]
                                                    , worstGrade   = Nothing
                                                    , bestGrade    = Nothing}

                          | otherwise = StudentsLog { studentNames = map name list
                                                    , worstGrade   = Just (minimum . map grade $ list)
                                                    , bestGrade    = Just (maximum . map grade $ list)}

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--
calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' = foldMap (\ x -> StudentsLog [name x] (pure $ grade x) (pure $ grade x))

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)
-- Сделайте 'Tree' представителем класса типов 'Foldable'

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf                  = mempty
    foldMap f (Node value children) = mconcat $ f value : map (foldMap f) children

-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1 балл)

data Apple = Apple
    { color  :: String -- цвет яблока
    , weight :: Float  -- вес яблока
    }
  deriving (Eq, Show)

instance Ord Apple where                              -- для heaviestApple
  (<=) :: Apple -> Apple -> Bool
  (<=) apple1 apple2 = weight apple1 <= weight apple2

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится 
--     в заданном диапазоне весов (0,25 балла)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree (l, r) = getAll . foldMap (\apple -> All ((weight apple <= r) && (weight apple >= l))) $ tree

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple Leaf = Nothing
heaviestApple tree = Just . maximum $ tree

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--
thisApple :: Tree Apple -> [String] -> (Int, Int) -> Maybe Apple
thisApple Leaf                  _      _      = Nothing
thisApple tree colors (l, r) = getFirst $ foldMap (First . f) tree
  where
    f :: Apple -> Maybe Apple
    f apple = if (weight apple <= fromIntegral r) && (weight apple >= fromIntegral l) && elem (color apple) colors
                then Just apple
                else Nothing

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
collectBasket Leaf = Basket { apples = M.empty }
collectBasket tree = Basket . M.map sort . M.fromListWith (<>) . foldMap (\x -> [(color x, [x])]) $ tree

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
siftDown heap
  | heap == BinLeaf = heap
  | left heap /= BinLeaf && (val . left $ heap) < val heap &&
     (right heap == BinLeaf || ((val . right $ heap) >= (val . left $ heap))) = processLeft heap
  | right heap /= BinLeaf && (val . right $ heap) < val heap = processRight heap
  | otherwise = heap
  where
    processLeft :: Ord a => BinaryHeap a -> BinaryHeap a
    processLeft BinLeaf = error "Unreachable statement"
    processLeft BinNode {val=v, left=l, right=r} =
      BinNode  { val   = val l
                  , left  = siftDown BinNode {val = v, left = left l, right = right l}
                  , right = r}
    processRight :: Ord a => BinaryHeap a -> BinaryHeap a
    processRight BinLeaf = error "Unreachable statement"
    processRight BinNode {val=v, left=l, right=r} =
      BinNode  { val   = val r
                  , left  = l
                  , right = siftDown BinNode {val = v, left = left r, right = right r}}

-- 6.b Реализуйте с помощью свёртки функцию buildHeap,
--     которая за __линейное время__ конструирует на основе спиcка элементов бинарную кучу.
--     Соответствующий алогритм описан в статье на вики (ссылка выше).
--     Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!)
--     (1 балл)
--       
-- создаем N пустых куч, идем справа налево (индексация с 1)
-- на каждом шаге добавляем левого ребенка -> корректная куча с индексом 2 * i, правый -> 2 * i + 1
-- если индекс за пределами -> BinLeaf
-- после добавления детей siftDown и куча i корректна

buildHeap :: Ord a => [a] -> BinaryHeap a
buildHeap arr = foldl' insert emptyArr (reverse $ zip [1..] arr) A.! 1
  where
    len :: Int
    len = length arr

    emptyArr :: A.Array Int (BinaryHeap a)
    emptyArr = A.listArray (1, len) (replicate len BinLeaf) -- важно! сами создали индексацию

    insert :: Ord a => A.Array Int (BinaryHeap a) -> (Int, a) -> A.Array Int (BinaryHeap a)
    insert acc (i, x) = acc A.// [(i, siftDown $ BinNode x (getChild (2 * i) acc) (getChild (2 * i + 1) acc))]

    getChild :: Int -> A.Array Int (BinaryHeap a) -> BinaryHeap a
    getChild i acc = if i <= len then acc A.! i else BinLeaf

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
toList (BLeaf   _ value)              = [value]
toList (BBranch _ leftTree rightTree) = (<>) (toList leftTree) (toList rightTree)

-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)

tag :: BinaryTree v a -> v
tag (BLeaf   v _  ) = v
tag (BBranch v _ _) = v

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
--
head' :: BinaryTree v a -> a
head' (BLeaf _ value)      = value
head' (BBranch _ leftTree _) = head' leftTree

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

myTree :: BinaryTree Size String
myTree = branchSize (leafSize "a") (branchSize (branchSize (leafSize "b") (leafSize "c")) (leafSize "d"))

-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ value)                ind | ind /= 1 = error "Incorrect number of list!"
                                          | otherwise          = value
getInd (BBranch _ leftTree rightTree) ind | tag leftTree < ind = getInd rightTree (ind - tag leftTree)
                                          | otherwise          = getInd leftTree ind

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

leafPrio :: Int -> a -> BinaryTree Priority a
leafPrio = BLeaf

branchPrio :: BinaryTree Priority a -> BinaryTree Priority a -> BinaryTree Priority a
branchPrio x y = BBranch (min (tag x) (tag y)) x y

-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)

myPrioTree :: BinaryTree Priority String
myPrioTree = branchPrio (leafPrio 5 "a") (branchPrio (branchPrio (leafPrio 11 "b") (leafPrio 3 "c")) (leafPrio 7 "d"))

-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: BinaryTree Priority a -> a
getWinner (BLeaf _ value) = value
getWinner (BBranch t leftTree rightTree) | t == tag leftTree = getWinner leftTree
                                         | otherwise         = getWinner rightTree

-------------------------------------------------------------------------------

-- 9. BinaryTree и Monoid (1 балла)

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

-- 9.a Обобщите эту функцию, написав инстансы `Monoid Size` и `Monoid Priority` (0,5 балла)

-- Kакую ошибку вы получили с при текущем определении Size и Priority, попытавшись создать инстансы?
-- Duplicate instance declarations

-- Что нужно изменить в определении Size и Priority?
-- записать их как newtype:

newtype MySize = MySize { getMySize :: Int } deriving (Show, Eq)
newtype MyPriority = MyPriority { getMyPriority :: Int } deriving (Show, Eq)

instance Semigroup MySize where
  (<>) :: MySize -> MySize -> MySize
  x <> y = MySize $ getMySize x + getMySize y

instance Monoid MySize where
  mempty :: MySize
  mempty = MySize 0 -- поправила, раньше поставила 1 по аналогии с листами, 
  -- но законы не выполнялись, так что исправила значение на 0
  -- x <> mempty = MySize $ getMySize x + getMySize mempty = MySize $ getMySize x + 1 /= x

instance Semigroup MyPriority where
  (<>) :: MyPriority -> MyPriority -> MyPriority
  x <> y = MyPriority $ min (getMyPriority x) (getMyPriority y)

instance Monoid MyPriority where
  mempty :: MyPriority
  mempty = MyPriority (maxBound :: Int)

-- Right identity
-- x <> mempty = MySize $ getMySize x + getMySize mempty = MySize $ getMySize x + 0 == x

-- Left identity
-- mempty <> x = MySize $ getMySize mempty + getMySize x = MySize $ 0 + getMySize x == x

-- Associativity
-- x <> (y <> z) = (x <> y) <> z (Semigroup law)

-- MySize $ getMySize x + getMySize (MySize $ getMySize y + getMySize z) -- left  parth
-- MySize $ getMySize (MySize $ getMySize x + getMySize y) + getMySize x -- right parth

-- раскрываем скобки:   getMySize (MySize a) = a
-- MySize $ getMySize x + getMySize y + getMySize x == MySize $ getMySize x + getMySize y + getMySize z

-- Concatenation
-- mconcat = foldr (<>) mempty
-- mappend = (<>) -- по определению, мы это не меняли
-- mconcat = foldr mappend mempty

-- | Теперь branchSize и branchPrio могут быть заменены на branch
--
branch :: Monoid v => BinaryTree v a -> BinaryTree v a -> BinaryTree v a
branch x y = BBranch (tag x <> tag y) x y

-- | Однако, мы не можем сделать то же с leaf. Почему?
-- Это не будет корректным, потому что mempty - значение, присваиваемое пустому множеству. 
-- Лист же таковым не является.
--
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

-- 9.b Напишите инстансы Measured для Size и Priority (0,5 балла)

instance Measured MySize a where
  measure :: a -> MySize
  measure _ = MySize {getMySize = 1}

instance (Enum a) => Measured MyPriority a where
  measure :: (Enum a) => a -> MyPriority
  measure priority = MyPriority $ fromEnum priority

-------------------------------------------------------------------------------
