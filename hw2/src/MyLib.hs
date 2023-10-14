module MyLib where

-- импортируем foldl', чтобы можно было вызывать как L.foldl
import qualified Data.List as L (foldl')

------------------------------------------------------------------------------------------------

-- 1. traceFoldl (0,25 балла)

-- Клёвый пакет для дебага хаскельных программ.
-- Например, функция traceShow имеет тип Show a => a -> b -> b.
-- Первым аргументом в неё нужно передать то, что хочется вывести на экран.
-- В остальном она работет так же, как и id. 
-- Поэтому её можно воткнуть в любое место в программе.
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
traceFoldl f acc xs = 
  let 
    y  = last xs
    ys = init xs
    res = traceFoldl f acc ys
  in f (traceShow res res) y

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с иcпользованием свертки (2,5 балла = 0,25 * 10)
-- Подумайте, какую функцию сверки когда лучше использовать

or' :: [Bool] -> Bool
or' = foldr (||) False
-- Тк True || _ == True нам нужно чтобы or' использовал правую свертку и был ленивым

length' :: [a] -> Int
length' = L.foldl' (\x _ -> x + 1) 0
-- Все равно проходить по всем элементам. Можем и без ленивости

maximum' :: [Int] -> Maybe Int
maximum' = L.foldl' (\x y -> if x > Just y then x else Just y) Nothing
-- Оказывается (Just 3) > Nothing == True. ¯\_(ツ)_/¯

reverse' :: [a] -> [a]
reverse' = L.foldl' (\x y -> y : x) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x y -> if f x then x : y else y) []
-- С левой сверткой не заходил

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

head' :: [a] -> Maybe a
head' = foldr (\x _ -> Just x) Nothing

last' :: [a] -> Maybe a
last' = foldl (\_ y -> Just y) Nothing

-- используйте L.foldl'
take' :: Int -> [a] -> [a]
take' n = L.foldl' (\x y -> if length' x < n then x ++ [y] else x) []

-- используйте foldr
take'' :: Int -> [a] -> [a]
take'' n = foldr (\x y -> if length' y < n then x : y else y) []

------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла = 1 + 1 + 0,5)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]


-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше
--   (1 балл)
--
insert :: Ord a => [a] -> a -> [a]
insert xs val = foldr helper [val] xs
  where helper x []       = [x]
        helper x (y : ys) = if x <= val then x : y : ys else y : x : ys

-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали
--
insertionSort :: Ord a => [a] -> [a]
insertionSort = L.foldl' insert []

------------------------------------------------------------------------------------------------

-- 4. Числа Фибоначчи (2,5 баллa = 0,5 + 1 + 1)

-- | аналогична zip, но применяет к элементам пары функцию (0,5 балла)
--
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
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
fibs = 0 : 1 : myZipWith (+) fibs (tail fibs)

-- | Считает сумму всех элементов списка, которые находятся на позиции, 
--     __индекс__ которой является числом Фибоначчи. (1 балл)
--

fibSum :: [Int] -> Int
fibSum xs = sum [x | (x, y) <- zip xs [0..], isInFibs y]

-- Показывает есть ли число в последовательности Фибоначчи
isInFibs :: Integer -> Bool
isInFibs n = helper n fibs
  where helper _ [] = False
        helper ind (x:xs) | ind == x  = True
                          | ind < x   = False
                          | otherwise = helper n xs

------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла = 0,25 + 1 + 1)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков. (0,25 балла)
--
data Tree a = Node {value :: a, children  :: [Tree a]}
  deriving (Show, Eq)

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs tree = map' value (helper [tree])
  where
    helper [] = []
    helper nodes = nodes ++ helper (concatMap children nodes)

  
-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs tree = map' value (helper [tree])
  where
    helper [] = []
    helper (node : nodes) = node : helper (children node ++ nodes)

------------------------------------------------------------------------------------------------