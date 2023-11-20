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
import Data.Maybe (isNothing)

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
traceFoldl _ acc []     = acc
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

-- Используем foldr, потому что хотим лениво исполниться при встрече первого True
or' :: [Bool] -> Bool
or' = foldr (||) False 

-- Здесь как-будто бы без разницы foldr/foldl, используем  более быструю функцию
length' :: [a] -> Int
length' = L.foldl' (\acc _ -> succ acc) 0

-- Используем более быструю - foldl', на длинных списках foldr приводит к stack overflow
maximum' :: [Int] -> Maybe Int
maximum' = L.foldl' (\acc x -> if isNothing acc then Just x else max acc (Just x)) Nothing 

-- Используем более быструю - foldl'
reverse' :: [a] -> [a]
reverse' = L.foldl' (flip (:)) []

-- Используем foldr, foldl' работает медленнее, видимо из-за того, что нужно делать (++)
filter' :: (a -> Bool) -> [a] -> [a]
filter' cond = foldr (\x acc -> if cond x then x : acc else acc) []
--filter' cond lst = reverse (L.foldl' (\acc x -> if cond x then x : acc else acc) [] lst)


-- Используем foldr, потому что работает быстрее
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []
-- map' f = L.foldl' (\ini x -> ini ++ [f x]) []

-- Интуитивнее foldr
head' :: [a] -> Maybe a
head' = foldr (\x _ -> Just x) Nothing

-- Интуитивнее foldl
last' :: [a] -> Maybe a
last' = L.foldl' (\_ x -> Just x) Nothing

-- используйте L.foldl'
takeL :: Int -> [a] -> [a]
takeL l = L.foldl' (\acc x -> if length acc < l then acc ++ [x] else acc) []

-- используйте foldr
takeR :: Int -> [a] -> [a]
takeR r = foldr (\x acc -> if length acc < r then x : acc else acc) []

------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort []       = []
quicksort (x:xs)   = quicksort (filter' (<x) xs) ++ [x] ++ quicksort (filter' (>x) xs) 

-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше (1 балл)
--
insert :: Ord a => [a] -> a -> [a]
insert lst x = filter' (<=x) lst ++ [x] ++ filter' (>x) lst

-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали
--
insertionSort :: Ord a => [a] -> [a]
insertionSort = L.foldl' insert []

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

-- Попробовала запустить с takeL - работает бесконечно долго, в целом, понятно почему
-- Посмотрела на реализацию take, там сразу видно, почему take умеет останавливаться
-- 
vals0123 :: [Int]
vals0123 = take 4 infList -- вернет [0..3]

-- | Задайте бесконечный список всех чисел Фибоначчи (1 балл)
--   Элементы списка должны вчисляться за O(n), где n — номер числа Фибоначчи
--   Подсказка: используйте myZipWith
--
fibs :: [Int]
fibs = 0 : fibsNext
  where
   fibsNext :: [Int]
   fibsNext = 1 : myZipWith (+) fibs fibsNext

-- | Считает сумму всех элементов списка, которые находятся на позиции, 
--     __индекс__ которой является числом Фибоначчи (1 балл)
--
fibSum :: [Int] -> Int
fibSum arr = sum [x | (x, idx) <- zip arr [0..], inFibs idx]

inFibs :: Int -> Bool
inFibs = helper fibs
  where
   helper [] _ = False 
   helper (x:xs) idx | idx == x  = True
                     | idx < x   = False
                     | otherwise = helper xs idx

------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков (0,25 балла)
--
data Tree a = Leaf
  | Node 
    { nodeValue  :: a,
      nodeChildren   :: [Tree a]
    }
  deriving (Show, Eq)

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs tree = helper [tree]
  where
   helper :: [Tree a] -> [a]
   helper treeList = 
      if null onlyNodes
        then []
        else map nodeValue onlyNodes ++ helper (concatMap nodeChildren onlyNodes)
    where 
      onlyNodes = filter isNode treeList
        where 
          isNode :: Tree a -> Bool
          isNode Leaf = False
          isNode _    = True

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs Leaf = []
dfs (Node val ch) = val : L.foldl' (\acc node -> acc ++ dfs node) [] ch
------------------------------------------------------------------------------------------------
