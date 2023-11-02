{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- для использования классов типов с несколькими параметрами
{-# LANGUAGE TypeSynonymInstances #-} --  чтобы можно было спользовать синонимы типов в объявлениях экземпляров
{-# LANGUAGE FlexibleInstances #-} 


module MyLib where

import qualified Data.Map.Strict as M
import Data.List (sortOn)
import Data.Monoid ()
import Data.Array

-- Во всех заданиях с инстансами укажите сигнатуры функций

-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
newtype Arrow a b = Arrow (a -> b)

instance Functor (Arrow a) where
    fmap :: (a2 -> b) -> Arrow a1 a2 -> Arrow a1 b
    fmap f (Arrow g) = Arrow (f . g)

-- Напишите инстанс Functor для Arrow и покажите выполнение законов
-- 1. Functors must preserve identity morphisms
-- fmap id = id
-- 2. Functors preserve composition of morphisms
-- fmap (f . g) == fmap f . fmap g

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
calculateStudentsLog students = StudentsLog
    { studentNames = map name students
    , worstGrade   = if null students then Nothing else Just (minimum (map grade students))
    , bestGrade    = if null students then Nothing else Just (maximum (map grade students))
    }

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--
instance Semigroup StudentsLog where
    (<>) :: StudentsLog -> StudentsLog -> StudentsLog
    a <> b = StudentsLog
        { studentNames = studentNames a ++ studentNames b
        , worstGrade   = case (worstGrade a, worstGrade b) of
                            (Nothing, x) -> x
                            (x, Nothing) -> x
                            (Just x, Just y) -> Just (min x y)
        , bestGrade    = case (bestGrade a, bestGrade b) of
                            (Nothing, x) -> x
                            (x, Nothing) -> x
                            (Just x, Just y) -> Just (max x y)
        }


instance Monoid StudentsLog where
    mempty :: StudentsLog
    mempty = StudentsLog [] Nothing Nothing
    mappend :: StudentsLog -> StudentsLog -> StudentsLog
    mappend = (<>)

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' = mconcat . map (\s -> StudentsLog [name s] (Just $ grade s) (Just $ grade s))

-- чтобы тесты работали нужны инстансы Show и Eq
instance Show StudentsLog where
    show :: StudentsLog -> String
    show (StudentsLog names worst best) = "StudentsLog {studentNames = " ++ show names ++ ", worstGrade = " ++ show worst ++ ", bestGrade = " ++ show best ++ "}"
    
instance Eq StudentsLog where
    (==) :: StudentsLog -> StudentsLog -> Bool
    (StudentsLog names1 worst1 best1) == (StudentsLog names2 worst2 best2) = names1 == names2 && worst1 == worst2 && best1 == best2

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

-- Сделайте 'Tree' представителем класса типов 'Foldable'

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Node x ts) = mappend (f x) (foldMap (foldMap f) ts)

-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1 балл)

data Apple = Apple
    { color  :: String -- цвет яблока
    , weight :: Float  -- вес яблока
    }
  deriving (Eq, Show)

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится 
--     в заданном диапазоне весов (0,25 балла)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree (low, high) = all inRange (foldMap (\apple -> [weight apple]) tree)
  where
    inRange x = x >= low && x <= high

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple = foldr maxApple Nothing
  where
    maxApple apple Nothing = Just apple
    maxApple apple (Just heaviest) = Just (if weight apple > weight heaviest then apple else heaviest)

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--
thisApple :: Tree Apple -> [String] -> (Float, Float) -> Maybe Apple -- я исправила (Int, Int) на (Float, Float) так как вроде и в части а он был такой + логично что диапазон нецелый
thisApple tree colors (low, high) = foldr matchApple Nothing tree
  where
    matchApple apple Nothing 
      | color apple `elem` colors && weight apple >= low && weight apple <= high = Just apple
      | otherwise = Nothing
    matchApple apple acc@(Just _) 
      | color apple `elem` colors && weight apple >= low && weight apple <= high = acc
      | otherwise = Just apple

-- 4.d Считает сумму весов всех яблок в дереве (0,25 балла)
--
sumOfApples :: Tree Apple -> Float
sumOfApples = foldr (\apple acc -> weight apple + acc) 0.0

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
collectBasket tree = Basket $ foldr insertApple M.empty tree
  where
    insertApple apple basket =
      let colorKey = color apple
          updatedApples = sortOn weight (apple : M.findWithDefault [] colorKey basket)
      in M.insert colorKey updatedApples basket

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
siftDown heap@(BinNode x left' right')
  | x <= minChildVal = heap
  | otherwise = BinNode minChildVal (siftDown $ replaceMinChild x) right'
  where
    minChildVal = min (rootVal left') (rootVal right')
    replaceMinChild val' = if rootVal left' == minChildVal then replaceVal left' val' else replaceVal right' val'
    rootVal BinLeaf = x
    rootVal (BinNode y _ _) = y
    replaceVal (BinNode _ l r) v = BinNode v l r
    replaceVal BinLeaf _ = BinLeaf

-- 6.b Реализуйте с помощью свёртки функцию buildHeap,
--     которая за __линейное время__ конструирует на основе спиcка элементов бинарную кучу.
--     Соответствующий алогритм описан в статье на вики (ссылка выше).
--     Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!)
--     (1 балл)
--       
-- Оно не работает 

buildHeap :: Ord a => [a] -> BinaryHeap a
buildHeap xs 
    | null xs = BinLeaf
    | otherwise = 
        let arr = listArray (1, length xs) xs
            heapSize = length xs
        in build arr heapSize (heapSize `div` 2)
    where
        build arr heapSize i
            | i < 1     = if heapSize > 0 then BinNode (arr ! 1) BinLeaf BinLeaf else BinLeaf
            | otherwise = siftDown $ BinNode (arr ! i) (buildChild arr heapSize (2 * i)) (buildChild arr heapSize (2 * i + 1))
        buildChild arr heapSize j = if j <= heapSize then build arr heapSize j else BinLeaf

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
toList (BLeaf _ a) = [a]
toList (BBranch _ left' right') = toList left' ++ toList right'

-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)

tag :: BinaryTree v a -> v
tag (BLeaf v _) = v
tag (BBranch v _ _) = v

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
--
head' :: BinaryTree v a -> a
head' (BLeaf _ a) = a
head' (BBranch _ left' _) = head' left'

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



-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ a) _ = a
getInd (BBranch _ left' right') n
  | n <= sizeLeft = getInd left' n
  | otherwise     = getInd right' (n - sizeLeft)
  where sizeLeft = tag left'

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
branchPrio x y = BBranch (tag x `min` tag y) x y


-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)



-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: BinaryTree Priority a -> a
getWinner (BLeaf _ a) = a
getWinner (BBranch _ _ a) = getWinner a

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
-- Kакую ошибку вы получили с при текущем определении Size и Priority, попытавшись создать инстансы? - Multiple declarations of ‘Size’ и Multiple declarations of ‘Priority’
-- Что нужно изменить в определении Size и Priority?  - переназвать их 

newtype Size' = Size' Int
  deriving (Show)

instance Semigroup Size' where
  (<>) :: Size' -> Size' -> Size'
  Size' x <> Size' y = Size' (x + y)

instance Monoid Size' where
  mempty :: Size'
  mempty = Size' 0

newtype Priority' = Priority' Int
  deriving (Show)

instance Semigroup Priority' where
  (<>) :: Priority' -> Priority' -> Priority'
  Priority' x <> Priority' y = Priority' (x `min` y)

instance Monoid Priority' where
  mempty :: Priority'
  mempty = Priority' maxBound 



-- | Теперь branchSize и branchPrio могут быть заменены на branch
--
branch :: Monoid v => BinaryTree v a -> BinaryTree v a -> BinaryTree v a
branch x y = BBranch (tag x <> tag y) x y

-- | Однако, мы не можем сделать то же с leaf. Почему?
--потому что mempty в контексте этой функции не имеет отношения к значению типа a, которое должно быть сохранено в листе дерева.
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
instance Measured Size' a where
  measure :: a -> Size'
  measure _ = Size' 1

instance (Ord a, Enum a) => Measured Priority' a where
  measure :: (Ord a, Enum a) => a -> Priority'
  measure x = Priority' (fromEnum x)


-------------------------------------------------------------------------------
