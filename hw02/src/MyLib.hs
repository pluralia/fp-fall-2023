module MyLib where

-- импортируем foldl', чтобы можно было вызывать как L.foldl'
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
  let acc' = f acc x
  in traceShow acc' $ traceFoldl f acc' xs

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла)
-- Подумайте, какую функцию сверки когда лучше использовать

or' :: [Bool] -> Bool
or' = foldr (||) False 

length' :: [a] -> Int
length' = L.foldl' (\acc _ -> acc + 1) 0

maximum' :: [Int] -> Maybe Int
maximum' []       = Nothing
maximum' (x : xs) = Just $ L.foldl' max x xs

reverse' :: [a] -> [a]
reverse' = L.foldl' (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = L.foldl' (\l x -> if f x then l ++ [x] else l) []

map' :: (a -> b) -> [a] -> [b]
map' f = L.foldl' (\l x -> l ++ [f x]) []

head' :: [a] -> Maybe a
head' = foldr (\x _ -> Just x) Nothing

last' :: [a] -> Maybe a
last' = L.foldl' (\_ x -> Just x) Nothing

-- используйте L.foldl'
takeL :: Int -> [a] -> [a]
takeL n = L.foldl' (\l x -> if length' l < n then l ++ [x] else l) []

-- используйте foldr
takeR :: Int -> [a] -> [a]
takeR n = foldr (\x l -> if length' l < n then x : l else l) []

------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort []       = []
quicksort (x : xs) = quicksort leftArray ++ [x] ++ quicksort rightArray
  where 
    leftArray  = filter (<= x) xs
    rightArray = filter (> x) xs

-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше (1 балл)
--

insert :: Ord a => [a] -> a -> [a]
insert [] x = [x]
insert (y : ys) x | x <= y    = x : y : ys
                  | otherwise = y : insert ys x

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
myZipWith _ [] _              = []
myZipWith _ _ []              = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

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
fibs = 0 : 1 : myZipWith (+) fibs (drop 1 fibs)

-- | Считает сумму всех элементов списка, которые находятся на позиции, 
--     __индекс__ которой является числом Фибоначчи (1 балл)
--
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
--
data Tree a = Node a [Tree a]

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs tree = bfs' [tree]
  where
    bfs' :: [Tree a] -> [a]
    bfs' []                        = []
    bfs' xs                        = map nodeValue xs ++ bfs' (concatMap nodeChildren xs)
    nodeValue (Node val _)         = val
    nodeChildren (Node _ children) = children

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs (Node val children) = val : concatMap dfs children

------------------------------------------------------------------------------------------------
