-- hlint предложил удалить прагму, предполагаю, что она нужна для заданий, с которыми я не разобралась, поэтому не убираю, 
-- т.к. в общем случае она может быть нужна
{-# LANGUAGE InstanceSigs #-}

module MyLib where

import qualified Data.Map.Strict as M

import Data.Monoid

-- Во всех заданиях с инстансами укажите сигнатуры функций

-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)

-------------------------------------------------------------------------------

-- 1. Functor для (->) (0,5 балла)

-- | Arrow представляет собой обертку над функцией из a в b
--
newtype Arrow a b = Arrow (a -> b)

-- Напишите инстанс Functor для Arrow и покажите выполнение законов
instance Functor (Arrow f) where
    fmap :: (x -> y) -> Arrow f x -> Arrow f y
    fmap f (Arrow g) = Arrow (f . g)

-- laws from https://wiki.haskell.org/Functor
-- 1
-- fmap id = id => fmap id (Arrow f) = Arrow (id . f) = Arrow f
-- 2
-- fmap (f . g)  ==  fmap f . fmap g => ???? я не поняла, что именно здесь нужно. это закон вроде из 25 строчки вытекает
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
calculateStudentsLog studentsList = StudentsLog {
    studentNames = if null studentsList then [] else map name studentsList
  , worstGrade = if null studentsList then Nothing else Just (minimum (map grade studentsList))
  , bestGrade = if null studentsList then Nothing else Just (maximum (map grade studentsList))
}

-- 2.b Сделайте 'StudentsLog' представителем класса типов 'Monoid' и реализуйте
--     calculateStudentsLog', которая делает то же самое, что и calculateStudentsLog
--     В реализации нужно использовать то, что 'StudentsLog' — моноид. (0,5 балла)
--

instance Semigroup StudentsLog where
    (<>) :: StudentsLog -> StudentsLog -> StudentsLog
    StudentsLog n1 w1 b1 <> StudentsLog n2 w2 b2 = StudentsLog (n1++n2) (getWorstGrade w1 w2) (getBestGrade b1 b2)
      where
        getWorstGrade :: Maybe Int -> Maybe Int -> Maybe Int
        getWorstGrade Nothing Nothing = Nothing
        getWorstGrade (Just a) Nothing = Just a
        getWorstGrade Nothing (Just b) = Just b
        getWorstGrade (Just a) (Just b) = Just (min a b)

        getBestGrade :: Maybe Int -> Maybe Int -> Maybe Int
        getBestGrade Nothing Nothing = Nothing
        getBestGrade (Just a) Nothing = Just a
        getBestGrade Nothing (Just b) = Just b
        getBestGrade (Just a) (Just b) = Just (max a b)

instance Monoid StudentsLog where
    mempty :: StudentsLog
    mempty = StudentsLog [] Nothing Nothing

calculateStudentsLog' :: [Student] -> StudentsLog
calculateStudentsLog' studentsList = mconcat [StudentsLog [name student] (Just (grade student)) (Just (grade student)) | student <- studentsList]

-------------------------------------------------------------------------------

-- 3. Дерево и Foldable (1 балл)

data Tree a = Node a [Tree a] | Leaf
  deriving (Eq, Show)

-- Сделайте 'Tree' представителем класса типов 'Foldable'

instance Foldable Tree where
    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf = mempty
    foldMap f (Node value child) = f value `mappend` foldMap (foldMap f) child

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
applesInRange tree (minW, maxW) = all (\apple -> minW <= weight apple && maxW >= weight apple) tree

-- 4.b Находит яблоко с наибольшим весом (0,25 балла)
--
heaviestApple :: Tree Apple -> Maybe Apple
heaviestApple = foldr maxWApple Nothing
  where
    maxWApple :: Apple -> Maybe Apple -> Maybe Apple
    maxWApple a Nothing = Just a
    maxWApple a1 (Just a2) = if weight a1 >= weight a2 then Just a1 else Just a2

-- 4.c Находит яблоко с цветом из заданного списка цветов и весом,
--     находящимся в заданном диапазоне весов (0,25 балла)
--
thisApple :: Tree Apple -> [String] -> (Float, Float) -> Maybe Apple
thisApple tree chosenColor (minW, maxW) = foldr chooseApple Nothing tree
  where
    chooseApple :: Apple -> Maybe Apple -> Maybe Apple
    chooseApple _ (Just b) = Just b
    -- здесь cabal поругался на -Wincomplete-patterns, но я не поняла, чем его тип не устроил
    chooseApple a Nothing = if color a `elem` chosenColor && minW <= weight a && maxW >= weight a
                            then Just a
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
collectBasket = Basket . foldr collectApples M.empty
  where
    collectApples :: Apple -> M.Map String [Apple] -> M.Map String [Apple]
    collectApples apple = M.insertWith (++) (color apple) [apple]

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
-- здесь cabal поругался на -Wname-shadowing, но я не стала менять названия для сохранения читаемости    
siftDown :: Ord a => BinaryHeap a -> BinaryHeap a
siftDown BinLeaf = BinLeaf
siftDown (BinNode v BinLeaf BinLeaf) = BinNode v BinLeaf BinLeaf
siftDown (BinNode v left BinLeaf) | v < val left = BinNode v left BinLeaf
                                  | otherwise = BinNode (val left) (siftDown (left {val = v})) BinLeaf
siftDown (BinNode v BinLeaf right)| v < val right = BinNode v BinLeaf right
                                  | otherwise = BinNode (val right) BinLeaf (siftDown (right {val = v}))
siftDown (BinNode v left right)   | v < min (val left) (val right) = BinNode v left right
                                  | val left < val right = BinNode (val left) (siftDown (left {val = v})) right
                                  | otherwise = BinNode (val right) left (siftDown (right {val = v}))

-- 6.b Реализуйте с помощью свёртки функцию buildHeap,
--     которая за __линейное время__ конструирует на основе спиcка элементов бинарную кучу.
--     Соответствующий алогритм описан в статье на вики (ссылка выше).
--     Считайте, что изменение элемента 'Data.Array' происходит за константу (хотя это не так!)
--     (1 балл)

-- судя по тому, что это пункт b, нужно использовать  siftDown, и я, кажется, даже поняла из статьи, что происходит, но не поняла, как это записать     
buildHeap :: Ord a => [a] -> BinaryHeap a
buildHeap = foldr undefined undefined 

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
toList (BBranch _ l r) = toList l ++ toList r

-- 7.b Реализуйте tag, возвращающую текущий тег дерева (0,25 балла)

tag :: BinaryTree v a -> v
tag (BLeaf v _) = v
tag (BBranch v _ _) = v

-- 7.c Реализуйте head, которая извлекает самый левый элемент (0,25 балла)
--
listHead :: BinaryTree v a -> a
listHead (BLeaf _ a) = a
listHead (BBranch _ l _) = listHead l

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

annotatedTree :: BinaryTree Size Char
annotatedTree = branchSize
                  (branchSize
                    (branchSize
                      (leafSize 'H')
                      (leafSize 'S')
                    )
                    (branchSize
                      (leafSize 'E')
                      (leafSize 'I')
                    )
                  )
                  (branchSize
                    (branchSize
                      (leafSize 'T')
                      (leafSize 'M')
                    )
                    (branchSize
                      (leafSize 'O')
                      (leafSize '!')
                    )
                  )
                  


-- 7.e Используя Size-аннотации, найдите n-й лист (1 балл)

-- здесь cabal тоже поругался на -Wincomplete-patterns, но я опять не поняла почему
getInd :: BinaryTree Size a -> Int -> a
getInd (BLeaf _ a) 1 = a
getInd (BBranch _ l r) n | n <= tag l = getInd l n
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
{-
class Monoid v => Measured v a where
    measure :: a -> v

-- | Написав различные инстансы этого класса для наших `BinaryTree Size a` и `BinaryTree Priority a`,
-- | мы сможем по-разному вычислять аннотацию листа по значению. Тогда
-- 
leaf :: Measured v a => a -> BinaryTree v a
leaf x = BLeaf (measure x) x
-}
-- 10.b Напишите инстансы Measured для Size и Priority (0,5 балла)

-------------------------------------------------------------------------------

