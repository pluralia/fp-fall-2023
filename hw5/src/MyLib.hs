{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingVia #-}

module MyLib where 

import qualified Data.Map.Strict as M
import Data.Monoid
-- import Data.List (foldl')

-- Во всех заданиях с инстансами укажите сигнатуры функций

-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
newtype Arrow a b = Arrow (a -> b)

instance Functor (Arrow a) where
  fmap :: (b -> c) -> Arrow a b -> Arrow a c
  fmap f (Arrow g) = Arrow (f . g)

-- Напишите инстанс Functor для Arrow и покажите выполнение законов
--  закон  fmap id      == id
-- fmap id (Arrow h) = Arrow (id . h) = Arrow h

-- закон fmap (g . h) == fmap g . fmap h
--  fmap (f . g) (Arrow h) = Arrow ((f . g) . h)
-- Arrow (f . (g . h)) = fmap f (fmap g (Arrow h))


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
  foldr helper (StudentsLog [] (Just maxBound) (Just minBound)) students
  where
    helper :: Student -> StudentsLog -> StudentsLog
    helper (Student n g) (StudentsLog names worst best) =
      StudentsLog (n : names) (min <$> worst <*> Just g) (max <$> best <*> Just g)




-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--
instance Semigroup StudentsLog where
  (StudentsLog names1 worst1 best1) <> (StudentsLog names2 worst2 best2) =
    StudentsLog (names1 ++ names2) (combine min worst1 worst2) (combine max best1 best2)
    where
      combine _ Nothing b = b
      combine _ a Nothing = a
      combine f (Just a) (Just b) = Just (f a b)

instance Monoid StudentsLog where
  mempty :: StudentsLog
  mempty = StudentsLog [] Nothing Nothing

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' = foldr mappend mempty . map studentToLog
  where
    studentToLog :: Student -> StudentsLog
    studentToLog (Student n g) = StudentsLog [n] (Just g) (Just g)

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node x children) = f x <> foldMap (foldMap f) children

-- Сделайте 'Tree' представителем класса типов 'Foldable'

-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1 балл)

data Apple = Apple
    { color  :: String -- цвет яблока
    , weight :: Float  -- вес яблока
    }
  deriving (Eq, Show)

instance Ord Apple where
    compare (Apple _ w1) (Apple _ w2) = compare w1 w2

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится 
--     в заданном диапазоне весов (0,25 балла)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange Leaf _ = True
applesInRange (Node (Apple _ weight) children) (low, high) =
  low <= weight && weight <= high && all (`applesInRange` (low, high)) children

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple Leaf = Nothing
heaviestApple (Node apple []) = Just apple
heaviestApple (Node apple subTrees) =
  let heaviestInSubTrees = foldr (max . heaviestApple) Nothing subTrees
  in max (Just apple) heaviestInSubTrees

maxApple :: Maybe Apple -> Maybe Apple -> Maybe Apple
maxApple Nothing y = y
maxApple x Nothing = x
maxApple (Just apple1) (Just apple2) =
  let weight1 = weight apple1
      weight2 = weight apple2
  in if weight1 >= weight2 then Just apple1 else Just apple2

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--
thisApple :: Tree Apple -> [String] -> (Int, Int) -> Maybe Apple
thisApple tree colors weightRange = getFirst $ foldMap checkApple tree
  where
    checkApple :: Apple -> First Apple
    checkApple a@(Apple c w)
      | c `elem` colors && wInRange w weightRange = First (Just a)
      | otherwise = First Nothing

    wInRange :: Float -> (Int, Int) -> Bool
    wInRange weight (low, high) = fromIntegral low <= weight && weight <= fromIntegral high

-- 4.d Считает сумму весов всех яблок в дереве (0,25 балла)
--
sumOfApples :: Tree Apple -> Float
sumOfApples = getSum . foldMap (\(Apple _ w) -> Sum w)

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
collectBasket = Basket . foldr insertApple M.empty
  where
    insertApple :: Apple -> M.Map String [Apple] -> M.Map String [Apple]
    insertApple (Apple color weight) basketMap =
      M.insertWith (++) color [Apple color weight] basketMap

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
siftDown :: Ord a => BinaryHeap a -> BinaryHeap a
siftDown BinLeaf = BinLeaf
siftDown node@(BinNode v l r) = case (l, r) of
    (BinLeaf, BinLeaf) -> node
    (BinLeaf, _) -> if v < val r
                       then BinNode v BinLeaf r
                       else BinNode (val r) BinLeaf (siftDown (r {val = v}))
    (_, BinLeaf) -> if v < val l
                       then BinNode v l BinLeaf
                       else BinNode (val l) (siftDown (l {val = v})) BinLeaf
    (_, _) ->
        let minChild = if val l < val r then l else r
        in if v < val minChild
               then BinNode v l r
               else BinNode (val minChild) (siftDown (minChild {val = v})) (if minChild == l then r else l)


-- 6.b Реализуйте с помощью свёртки функцию buildHeap,
--     которая за __линейное время__ конструирует на основе спиcка элементов бинарную кучу.
--     Соответствующий алогритм описан в статье на вики (ссылка выше).
--     Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!)
--     (1 балл)
--       
buildHeap :: Ord a => [a] -> BinaryHeap a
buildHeap l = foldr undefined undefined l

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
toList :: BinaryTree v a -> [a]
toList (BLeaf _ x) = [x]
toList (BBranch _ left right) = toList left ++ toList right


-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)

tag :: BinaryTree v a -> v
tag (BLeaf val _) = val
tag (BBranch val _ _) = val

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
--
head' :: BinaryTree v a -> a
head' (BLeaf _ x) = x
head' (BBranch _ left _) = head' left

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
create :: BinaryTree Size Char
create = 
  branchSize
    (branchSize (leafSize 'a') (leafSize 'b'))
    (branchSize (leafSize 'c') (leafSize 'd'))

-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ val) _ = val
-- getInd (BLeaf _ _) _ = error "Wrong indx"
getInd (BBranch _ left right) n
  | (n < 0) || (n > leftSize + 1) = error "Wrong"
  | n <= leftSize = getInd left n
  | otherwise = getInd right (n - leftSize)
  where
    leftSize = tag left

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
leafPrio p val = BLeaf p val

branchPrio :: BinaryTree Priority a -> BinaryTree Priority a -> BinaryTree Priority a
branchPrio x y = BBranch (tag x `min` tag y) x y

-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)
createPrioTree :: BinaryTree Priority Char
createPrioTree = 
  branchPrio
    (branchPrio (leafPrio 16 'a') (leafPrio 4 'a'))
    (branchPrio (leafPrio 2 'a') (leafPrio 8 'a'))

-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: BinaryTree Priority a -> a
getWinner (BLeaf _ val) = val
getWinner (BBranch _ left right)
  | tag left <= tag right = getWinner left
  | otherwise = getWinner right
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
-- Что нужно изменить в определении Size и Priority?

-- | Теперь branchSize и branchPrio могут быть заменены на branch
--
branch :: Monoid v => BinaryTree v a -> BinaryTree v a -> BinaryTree v a
branch x y = BBranch (tag x <> tag y) x y

-- | Однако, мы не можем сделать то же с leaf. Почему?
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

-- 10.b Напишите инстансы Measured для Size и Priority (0,5 балла)

-------------------------------------------------------------------------------
