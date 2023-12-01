{-# LANGUAGE InstanceSigs #-}

module MyLib where
import Data.Monoid
import Prelude hiding (head)
import qualified Data.Map.Strict as M
import qualified Data.List as L

-- Во всех заданиях с инстансами укажите сигнатуры функций

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
-- для тестов сделаем поле, чтобы была возможность достать саму функцию и применить 
newtype Arrow a b = Arrow (a -> b)

-- Напишите инстанс Functor для Arrow и покажите выполнение законов
instance Functor (Arrow a) where
    fmap :: (b -> c) -> Arrow a b -> Arrow a c
    fmap f (Arrow h) = Arrow (f . h)

-- Выполнение законов:
-- 1. fmap id == id
--    fmap id (Arrow h) = Arrow (id . h) = Arrow h
--
-- 2. fmap (f . g) == fmap f . fmap g
--    fmap (f . g) (Arrow h) = Arrow $ (f . g) . h = Arrow $ f . g . h
--    fmap g (Arrow h)       = Arrow $ g . h
--    fmap f (Arrow $ g . h) = Arrow $ f . g . h
-------------------------------------------------------------------------------

-- 2. Студенты и Monoid (1 балл)

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

-- 2.a Функция, которая по списку студентов курса рассчитывает информацию по курсу (0,5 балла)
--
calculateStudentsLog :: [Student] -> StudentsLog
calculateStudentsLog lst | null lst  = StudentsLog [] Nothing Nothing
                         | otherwise = StudentsLog
    { studentNames = map name lst
    , worstGrade   = Just $ minimum . map grade $ lst
    , bestGrade    = Just $ maximum . map grade $ lst
    }

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--

instance Semigroup StudentsLog where
    (<>) :: StudentsLog -> StudentsLog -> StudentsLog
    (<>) (StudentsLog n1 wg1 bg1) (StudentsLog n2 wg2 bg2) = StudentsLog
     { studentNames = n1 ++ n2
     , worstGrade   = minMaybe wg1 wg2
     , bestGrade    = maxMaybe bg1 bg2
    }
      where
        minMaybe :: Maybe Int -> Maybe Int -> Maybe Int
        minMaybe Nothing Nothing = Nothing
        minMaybe (Just x) Nothing = Just x
        minMaybe Nothing (Just y) = Just y
        minMaybe (Just x) (Just y) = Just (min x y)

        maxMaybe :: Maybe Int -> Maybe Int -> Maybe Int
        maxMaybe Nothing Nothing = Nothing
        maxMaybe (Just x) Nothing = Just x
        maxMaybe Nothing (Just y) = Just y
        maxMaybe (Just x) (Just y) = Just (max x y)

instance Monoid StudentsLog where
    mempty :: StudentsLog
    mempty = StudentsLog [] Nothing Nothing

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' lst = mconcat $ map (\s -> StudentsLog [name s] (Just $ grade s) (Just $ grade s)) lst

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)


-- Сделайте 'Tree' представителем класса типов 'Foldable'
instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf         = mempty
    foldMap toM (Node v c) = mappend (toM v) (mconcat . map (foldMap toM) $ c)

-------------------------------------------------------------------------------

-- 4. Яблоко и Foldable (1,5 балл)

data Apple = Apple
    { color  :: String -- цвет яблока
    , weight :: Float  -- вес яблока
    }
  deriving (Eq, Show)

-- С помощью функций из 'Data.Foldable' реализуйте следующие функции:

-- 4.a Проверка, что все яблоки в дереве имеют вес, который находится 
--     в заданном диапазоне весов (0,5 балла)
--
applesInRange :: Tree Apple -> (Float, Float) -> Bool
applesInRange tree (l, r) = getAll . foldMap (\a -> All (weight a >= l && weight a <= r)) $ tree

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
instance Ord Apple where
  (<=) :: Apple -> Apple -> Bool
  (<=) a1 a2 = weight a1 <= weight a2

heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple Leaf = Nothing
heaviestApple tree = Just $ maximum tree

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,5 балла)
--
thisApple :: Tree Apple -> [String] -> (Int, Int) -> Maybe Apple
thisApple tree colors (l, r) = foldr f Nothing tree
  where
    f :: Apple -> Maybe Apple -> Maybe Apple
    f apple acc = if (weight apple <= fromIntegral r) && (weight apple >= fromIntegral l) && elem (color apple) colors
      then Just apple
      else acc

-- 4.d Считает сумму весов всех яблок в дереве (0,25 балла)
--
sumOfApples :: Tree Apple -> Float
sumOfApples = getSum . foldMap (Sum . weight)

-------------------------------------------------------------------------------

-- 5. Корзинка с яблоками и Foldable (0,75 балла)

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
-- очень долго делала эту задачу :/ ломает мозг
--
collectBasket :: Tree Apple -> Basket
collectBasket tree = Basket $ M.map L.sort $ foldr diffApple (M.empty :: M.Map String [Apple]) tree
  where
    diffApple :: Apple -> M.Map String [Apple] -> M.Map String [Apple]
    diffApple a = M.insertWith (<>) (color a) [a]

-------------------------------------------------------------------------------

-- 6. Реализуйте функцию siftDown, восстанавливающую свойство кучи в куче (0,75 балла)
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

siftDown :: Ord a => BinaryHeap a -> BinaryHeap a
siftDown BinLeaf = BinLeaf
siftDown (BinNode v BinLeaf BinLeaf) = BinNode v BinLeaf BinLeaf
siftDown (BinNode v lnode BinLeaf) | v < val lnode                        = BinNode v lnode BinLeaf
                                   | otherwise                            = BinNode (val lnode) (siftDown lnode { val = v }) BinLeaf
siftDown (BinNode _ BinLeaf _)                                            = error "This is not a heap!"                                  
siftDown (BinNode v lnode rnode)   | (v < val lnode) && (v < val rnode)   = BinNode v lnode rnode
                                   | val lnode < val rnode                = BinNode (val lnode) (siftDown (lnode {val = v})) rnode 
                                   | otherwise                            = BinNode (val rnode) lnode (siftDown (rnode {val = v}))
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
toList (BBranch _ ltree rtree) = toList ltree <> toList rtree

-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)

tag :: BinaryTree v a -> v
tag (BLeaf tagv _) = tagv
tag (BBranch tagv _ _) = tagv

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
--
head :: BinaryTree v a -> a
head (BLeaf _ a) = a
head (BBranch _ ltree _) = head ltree

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
sizeTree :: BinaryTree Size String 
sizeTree  = branchSize (branchSize (leafSize "a") (leafSize "b")) (branchSize (branchSize (leafSize "c") (leafSize "e")) (leafSize "d"))
--
--           5
--        /     \
--       2       3
--     /  \    /  \
--    a    b  2    d
--           / \
--          c   e 

-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ v) n = if n == 1 then v else error "Wrong index!"        
getInd (BBranch _ ltree rtree) n | tag ltree < n      = getInd rtree (n - tag ltree)
                                 | otherwise          = getInd ltree n

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

-- 8.b Создайте дерево типа `BinaryTree Priority a`, используя leafPrio и branchPrio (0,25 балла)

-- 8.c Используя Priority-аннотации, найдите самый приоритетный элемент (1 балл)

getWinner :: BinaryTree Priority a -> a
getWinner = undefined

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

--class Monoid v => Measured v a where
--    measure :: a -> v

-- | Написав различные инстансы этого класса для наших `BinaryTree Size a` и `BinaryTree Priority a`,
-- | мы сможем по-разному вычислять аннотацию листа по значению. Тогда
-- 
--leaf :: Measured v a => a -> BinaryTree v a
--leaf x = BLeaf (measure x) x

-- 10.b Напишите инстансы Measured для Size и Priority (0,5 балла)

-------------------------------------------------------------------------------

