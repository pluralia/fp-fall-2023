module MyLib where
import qualified Data.List as L (foldl')

------------------------------------------------------------------------------------------------

-- 1. traceFoldl (0,25 балла)

-- | Клёвый пакет для дебага хаскельных программ.
--   Например, функция traceShow имеет тип Show a => a -> b -> b.
--   Первым аргументом в неё нужно передать то, что хочется вывести на экран.
--   В остальном она работет так же, как и id. 
--   Поэтому её можно воткнуть в любое место в программе.
--
import Debug.Trace (traceShow)

-- Пример
traceFoldr :: Show b => (a -> b -> b) -> b -> [a] -> b
traceFoldr _ acc []       = acc
traceFoldr f acc (x : xs) =
  let res = traceFoldr f acc xs
   in f x (traceShow res res)

-- | Напишите traceFoldl, аналогичную traceFoldr,
--   которая с помощью traceShow печатает значение аккумулятора на каждом шаге
--
traceFoldl :: Show b => (b -> a -> b) -> b -> [a] -> b
traceFoldl _ acc []       = acc
traceFoldl f acc (x : xs) =
    let res = f acc x
    in  traceShow res $ traceFoldl f res xs

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла)
-- Подумайте, какую функцию сверки когда лучше использовать

or' :: [Bool] -> Bool
or' = foldr (||) False


length' :: [a] -> Int
length' = L.foldl' (\acc _ -> acc + 1) 0

maximum' :: [Int] -> Maybe Int
maximum' [] = Nothing
maximum' (x:xs) = Just $ L.foldl' (\acc y -> if y > acc then y else acc) x xs

reverse' :: [a] -> [a]
reverse' = L.foldl' (\acc x -> x : acc) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = L.foldl' (\acc x -> if f x then acc ++ [x] else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = L.foldl' (\acc x -> acc ++ [f x]) []

head' :: [a] -> Maybe a
head' = L.foldl' (\acc x -> case acc of
                              Nothing -> Just x
                              _ -> acc) Nothing


last' :: [a] -> Maybe a
last' = L.foldl' (\_ y -> Just y) Nothing
-- используйте L.foldl'
takeL :: Int -> [a] -> [a]
takeL a = L.foldl' (\acc x -> if length' acc < a then acc ++ [x] else acc) []

-- используйте foldr
takeR :: Int -> [a] -> [a]
takeR a = foldr (\acc x -> if length x < a then acc : x else x) []
------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x]
                   ++ [x]
                   ++ quicksort [y | y <- xs, y > x]


-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше (1 балл)
--
insert :: Ord a => [a] -> a -> [a]
insert [] y = [y]
insert (x:xs) y
    | y <= x    = y : x : xs
    | otherwise = x : insert xs y

-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали
--
insertionSort :: Ord a => [a] -> [a]
insertionSort []       = []
insertionSort (x : xs) = insert (insertionSort xs) x

------------------------------------------------------------------------------------------------

-- 4. Числа Фибоначчи (2,5 баллa)

-- | аналогична zip, но применяет к элементам пары функцию (0,5 балла)
--
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys


-- | В Haskell можно создавать бесконечные списки.
--
infList :: [Int]
infList = helper 0
  where
    helper :: Int -> [Int]
    helper x = x : helper (x + 1)

-- | Из-за ленивости они будут работать.
--   В том плане, что они будут вычисляться ровно до того момента, до которого нам надо.
-- 
vals0123 :: [Int]
vals0123 = take 4 infList -- вернет [0..3]

-- | Задайте бесконечный список всех чисел Фибоначчи (1 балл)
--   Элементы списка должны вчисляться за O(n), где n — номер числа Фибоначчи
--   Подсказка: используйте myZipWith
--
fibs :: [Integer]
fibs = 0 : 1 : myZipWith (+) fibs (tail fibs)

-- | Считает сумму всех элементов списка, которые находятся на позиции, 
--     __индекс__ которой является числом Фибоначчи (1 балл)
--
fibSum :: [Integer] -> Integer
fibSum lst = foldr (\idx acc -> acc + lst !! fromIntegral idx) 0 (takeWhile (< length' lst) (map fromIntegral fibs))

------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков (0,25 балла)
--
data Tree a = Leaf a | Node [Tree a]
  deriving (Show, Eq)-- 'YourTree' — просто заглушка. Вместо неё вам надо будет написать своё описание типа данных 'Tree'

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs tree = help [tree]
  where
    help :: [Tree a] -> [a]
    help [] = []
    help (Leaf x : other) = x : help other
    help (Node child : other) = help (other ++ child)

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs (Leaf x) = [x]
dfs (Node child) = concatMap dfs child

------------------------------------------------------------------------------------------------
