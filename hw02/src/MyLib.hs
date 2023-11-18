module MyLib where

-- импортируем foldl', чтобы можно было вызывать как L.foldl'
-- import Data.List qualified as L (foldl')
-- ImportQualifiedPost
import qualified Data.List as L (foldl')
------------------------------------------------------------------------------------------------

-- 1. traceFoldl (0,25 балла)

-- \| Клёвый пакет для дебага хаскельных программ.
--   Например, функция traceShow имеет тип Show a => a -> b -> b.
--   Первым аргументом в неё нужно передать то, что хочется вывести на экран.
--   В остальном она работет так же, как и id.
--   Поэтому её можно воткнуть в любое место в программе.
import Debug.Trace (traceShow)

-- Пример
-- смотрит вглубь рекурсивно
traceFoldr :: (Show b) => (a -> b -> b) -> b -> [a] -> b
traceFoldr _ acc [] = acc
traceFoldr f acc (x : xs) =
  let res = traceFoldr f acc xs
   in f x (traceShow res res)

-- Напишите traceFoldl, аналогичную traceFoldr,
-- которая с помощью traceShow печатает значение аккумулятора на каждом шаге
-- считается сразу

traceFoldl :: (Show b) => (b -> a -> b) -> b -> [a] -> b
traceFoldl _ acc [] = acc
traceFoldl f acc (x : xs) =
  let res = f acc x
   in traceFoldl f (traceShow res res) xs

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла)
-- Подумайте, какую функцию сверки когда лучше использовать

-- foldr :: (a -> b -> b) -> b -> [a] -> [b]
-- foldr _ ini []     = ini
-- foldr f ini (x:xs) = f x (foldr f ini xs)

-- foldl :: (b -> a -> b) -> b -> [a] -> [b]
-- foldl _ ini []     = ini
-- foldl f ini (x:xs) = foldl f (f ini x) xs

--
-- без разницы
or' :: [Bool] -> Bool
or' = foldr (||) False

-- or' = foldr (||) False
-- or' = or

-- best choice is foldl
length' :: [a] -> Int
length' = foldl (\acc _ -> acc + 1) 0

-- best choice is foldl
maximum' :: [Int] -> Maybe Int
maximum' [] = Nothing
maximum' xs = traceFoldl helper Nothing xs
  where
    helper Nothing a = Just a
    helper (Just acc) a = Just (max acc a)

-- [1, 2, 3, 4, 5]
-- 1:(2:(3:(4:(5))))
-- 3 : (2 : 1 :)
-- :p
reverse' :: [a] -> [a]
reverse' = L.foldl' (flip (:)) []

--
filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldl (helper f) []
  where
    helper h acc a = if h a then acc ++ [a] else acc

-- filter' :: (a -> Bool) -> [a] -> [a]
-- filter' cond = foldr (\x acc -> if cond x then x : acc else acc) []
-- --filter' cond = L.foldl' (\acc x -> if cond x then acc ++ [x] else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (helper f) []
  where
    helper h acc el = acc ++ [h el]

head' :: [a] -> Maybe a
head' = foldl f Nothing
  where
    f Nothing el = Just el
    f (Just acc) _ = Just acc

last' :: [a] -> Maybe a
last' = foldl f Nothing
  where
    f Nothing el = Just el
    f (Just _) el = Just el

-- Интуитивнее foldr
-- head' :: [a] -> Maybe a
-- head' = foldr (\x _ -> Just x) Nothing

-- используйте L.foldl'
-- :p
takeL :: Int -> [a] -> [a]
takeL l = L.foldl' (\ini x -> if length ini < l then ini ++ [x] else ini) []

-- используйте foldr
-- :p
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

-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше (1 балл)
insert :: (Ord a) => [a] -> a -> [a]
insert arr x = filter' (<= x) arr ++ [x] ++ filter' (> x) arr

-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали
insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldl insert []

------------------------------------------------------------------------------------------------

-- 4. Числа Фибоначчи (2,5 баллa)

-- | аналогична zip, но применяет к элементам пары функцию (0,5 балла)
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

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

-- fibs :: [Integer]
-- fibs = helper (0, 1)
--   where
--     helper :: (Integer, Integer) -> [Integer]
--     helper (i0, i1) = i1 : helper (i1, i0 + i1)

fibsn :: Int -> [Integer]
fibsn n = take n fibs

fibs :: [Integer]
fibs = 0 : fibsNext
  where
    fibsNext :: [Integer]
    fibsNext = 1 : myZipWith (+) fibs fibsNext

-- | Считает сумму всех элементов списка, которые находятся на позиции,
--     __индекс__ которой является числом Фибоначчи (1 балл)

-- :p
-- fibSum :: [Int] -> Int
-- fibSum arr = sum [x | (x, idx) <- zip arr [0 ..], inFibs idx]

-- inFibs :: Int -> Bool
-- inFibs = helper fibs
--   where
--     helper [] _ = False
--     helper (x : xs) idx
--       | idx == x = True
--       | idx < x = False
--       | otherwise = helper xs idx

-- | Считает сумму всех элементов списка, которые находятся на позиции,
--     __индекс__ которой является числом Фибоначчи (1 балл)

-- :p
fibSum :: [Integer] -> Integer
fibSum list = foldr fHelper 0 (takeWhile (< len) fibs)
  where
    len = fromIntegral (length list)
    fHelper :: Integer -> Integer -> Integer
    fHelper idx acc = acc + (list !! fromIntegral idx)

------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков (0,25 балла)
data Tree a
  = Leaf
  | Node
      { nodeValue :: a,
        nodeChildren :: [Tree a]
      } -- 'YourTree' — просто заглушка. Вместо неё вам надо будет написать своё описание типа данных 'Tree'

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)

-- :p
bfs :: Tree a -> [a]
bfs tree = helper [tree]
  where
    helper :: [Tree a] -> [a]
    helper treeList = if null onlyNodes then [] else map nodeValue onlyNodes ++ helper (concatMap nodeChildren onlyNodes)
      where
        onlyNodes = filter isNode treeList

isNode :: Tree a -> Bool
isNode Leaf = False
isNode _ = True

dfs :: Tree a -> [a]
dfs Leaf = []
dfs (Node val ch) = val : L.foldl' (\acc node -> acc ++ dfs node) [] ch

------------------------------------------------------------------------------------------------