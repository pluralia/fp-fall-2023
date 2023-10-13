module MyLib where

-- импортируем foldl', чтобы можно было вызывать как L.foldl'
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
traceFoldl f acc (x : xs) = 
  let myAcc = f acc x
   in traceShow myAcc (traceFoldl f myAcc xs)

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла = 0,25 * 10)
-- Подумайте, какую функцию сверки когда лучше использовать

or' :: [Bool] -> Bool
or' = foldr (||) False

length' :: [a] -> Int
length' = L.foldl'(\count _ -> count + 1) 0

maximum' :: [Int] -> Maybe Int
maximum'[] = Nothing
maximum'(x : xs) = Just (L.foldl' max x xs)

reverse' :: [a] -> [a]
reverse' = L.foldl' (flip(:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' condition = L.foldl'(\acc x -> if condition x then x:acc else acc)[]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x:acc) []

head' :: [a] -> Maybe a
head' = foldr (\x _ -> Just x) Nothing

last' :: [a] -> Maybe a
last' [] = Nothing
last' (x:xs) = Just(L.foldl' (\_ el -> el) x xs)

-- используйте L.foldl'
take' :: Int -> [a] -> [a]
take' n = L.foldl'(\acc x -> if length' acc < n then acc ++ [x] else acc) []

-- используйте foldr
take'' :: Int -> [a] -> [a]
take'' n = foldr (\x acc -> if length' acc < n then x:acc else acc) []

------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла = 1 + 1 + 0,5)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x : xs) = quicksort [a | a <- xs, a <= x] ++ [x] ++ quicksort [a | a <- xs, a > x] 


-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше
--   (1 балл)
--
insert :: [Int] -> Int -> [Int]
insert [] a = [a]
insert (x:xs) a | a < x = a:x:xs
                | otherwise = x:insert xs a

-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали

-- вроде должно работать, но не работает, поэтому закомментила
-- insertionSort :: Ord a => [a] -> [a]
-- insertionSort [] = []
--insertionSort (x:xs) = insert(insertionSort xs) x

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
-- я понимаю, что нужно сделать, но не понимаю как мне это написать с использованием списка fibs
-- fibSum :: [Int] -> Int
-- fibSum = undefined

------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла = 0,25 + 1 + 1)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков. (0,25 балла)
--
data Tree a = Node a [Tree a] -- 'YourTree' — просто заглушка. Вместо неё вам надо будет написать своё описание типа данных 'Tree'

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs tree = bfsHelper [tree] []
  where
    bfsHelper [] acc = reverse acc
    bfsHelper (Node x children : rest) acc =
      bfsHelper (rest ++ children) (x : acc)

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs tree = dfsHelper [tree] []
  where
    dfsHelper [] acc = reverse acc
    dfsHelper (Node x children : rest) acc =
      dfsHelper (children ++ rest) (x : acc)
------------------------------------------------------------------------------------------------