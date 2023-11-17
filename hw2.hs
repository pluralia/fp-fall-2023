-- импортируем foldl', чтобы можно было вызывать как L.foldl'

------------------------------------------------------------------------------------------------

-- 1. traceFoldl (0,25 балла)

-- \| Клёвый пакет для дебага хаскельных программ.
--   Например, функция traceShow имеет тип Show a => a -> b -> b.
--   Первым аргументом в неё нужно передать то, что хочется вывести на экран.
--   В остальном она работет так же, как и id.
--   Поэтому её можно воткнуть в любое место в программе.

import Data.ByteString (partition)
import Data.List qualified as L (foldl')
import Debug.Trace (traceShow)

-- Пример
-- смотрит вглубь рекурсивно
-- traceFoldr :: (Show b) => (a -> b -> b) -> b -> [a] -> b
traceFoldr :: (a -> b -> b) -> b -> [a] -> b
traceFoldr _ acc [] = acc
traceFoldr f acc (x : xs) =
  let res = traceFoldr f acc xs
   in f x res

--  in f x (traceShow res res)

-- traceFoldr :: (a -> b -> b) -> b -> [a] -> b
-- traceFoldr _ acc [] = acc
-- traceFoldr f acc (x : xs) =
--   let res = traceFoldr f acc xs
--  in f x res

-- let are local and don't pass guards

-- Напишите traceFoldl, аналогичную traceFoldr,
-- которая с помощью traceShow печатает значение аккумулятора на каждом шаге
-- считается сразу

-- traceFoldl :: (Show b) => (b -> a -> b) -> b -> [a] -> b
traceFoldl :: (b -> a -> b) -> b -> [a] -> b
traceFoldl _ acc [] = acc
traceFoldl f acc (x : xs) = traceFoldl f (f acc x) xs

-- traceFoldl f acc (x : xs) = traceFoldl f (f (traceShow acc acc) x) xs

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла)
-- Подумайте, какую функцию сверки когда лучше использовать

-- best choice is foldl
-- or' arr = traceFoldl (||) acc arr

or' :: [Bool] -> Bool
or' = traceFoldl (||) acc
  where
    acc = False

-- best choice is foldl
length' :: [a] -> Int
length' = traceFoldl (\acc a -> acc + 1) acc
  where
    acc = 0

-- best choice is foldl
maximum' :: [Int] -> Maybe Int
maximum' [] = Nothing
maximum' xs = traceFoldl helper acc xs
  where
    acc = Nothing
    helper Nothing a = Just a
    helper (Just acc) a = Just (max acc a)

-- best choice is fold
-- reverse' :: [Int] -> [Int]
-- reverse' [] = []
-- reverse' xs = traceFoldr helper acc xs
--   where
--     acc = []
--     helper acc a = acc ++ a

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = traceFoldl (helper f) acc
  where
    acc = []
    helper f _ a = if f a then acc ++ [a] else acc

map' :: (a -> b) -> [a] -> [b]
map' f = traceFoldl (helper f) acc
  where
    acc = []
    helper f acc el = acc ++ [f el]

head' :: [a] -> Maybe a
head' = traceFoldl f acc
  where
    acc = Nothing
    f Nothing el = Just el
    f (Just acc) el = Just acc

last' :: [a] -> Maybe a
last' = traceFoldl f acc
  where
    acc = Nothing
    f Nothing el = Just el
    f (Just acc) el = Just el

-- используйте L.foldl'
takeL :: Int -> [a] -> [a]
takeL l = L.foldl' (\ini x -> if length ini < l then ini ++ [x] else ini) []


-- используйте foldr
takeR :: Int -> [a] -> [a]
takeR r = foldr (\x ini -> if length ini < r then x : ini else ini) []

------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла)

-- | Быстрая сортировка (1 балл)
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (p : xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

-- quicksort [] = []
-- quicksort [x] = [x]
-- quicksort arr = quicksort takeL lomuto x arr ++  [x] ++ quicksort takeR lomuto x arr
--   -- where
--   --   x = 0
--   --   lomuto arr = undefined

-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше (1 балл)

-- x is passed to a f
-- traceFoldl :: (b -> a -> b) -> b -> [a] -> b
-- traceFoldl _ acc [] = acc
-- traceFoldl f acc (x : xs) = traceFoldl f (f acc x) xs

insert :: Ord a => [a] -> a -> [a]
insert lst x = filter' (<=x) lst ++ [x] ++ filter' (>x) lst
-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = undefined

------------------------------------------------------------------------------------------------

-- 4. Числа Фибоначчи (2,5 баллa)

-- | аналогична zip, но применяет к элементам пары функцию (0,5 балла)
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

-- | В Haskell можно создавать бесконечные списки.
infList :: [Int]
infList = helper 0
  where
    helper :: Int -> [Int]
    helper x = x : helper (x + 1)

-- | Из-за ленивости они будут работать.
--   В том плане, что они будут вычисляться ровно до того момента, до которого нам надо.
vals0123 :: [Int]
vals0123 = take 4 infList -- вернет [0..3]

-- | Задайте бесконечный список всех чисел Фибоначчи (1 балл)
--   Элементы списка должны вчисляться за O(n), где n — номер числа Фибоначчи
--   Подсказка: используйте myZipWith
fibs :: [Integer]
fibs = helper (0, 1)
  where
    helper :: (Integer, Integer) -> [Integer]
    helper (i0, i1) = i1 : helper (i1, i0 + i1)

fibsn :: Int -> [Integer]
fibsn n = take n fibs

-- | Считает сумму всех элементов списка, которые находятся на позиции,
--     __индекс__ которой является числом Фибоначчи (1 балл)
fibSum :: [Int] -> Int
fibSum = undefined

------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков (0,25 балла)
data Tree a = YourTree -- 'YourTree' — просто заглушка. Вместо неё вам надо будет написать своё описание типа данных 'Tree'

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
bfs :: Tree a -> [a]
bfs = undefined

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
dfs :: Tree a -> [a]
dfs = undefined

------------------------------------------------------------------------------------------------