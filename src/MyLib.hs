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

-- Пример (заменил acc на ini потому что мне так нравится и понятно больше)
traceFoldr :: Show b => (a -> b -> b) -> b -> [a] -> b
traceFoldr _ ini []       = ini
traceFoldr f ini (x : xs) =
  let res = traceFoldr f ini xs
   in f x (traceShow res res)

-- | Напишите traceFoldl, аналогичную traceFoldr,
--   которая с помощью traceShow печатает значение аккумулятора на каждом шаге
--
traceFoldl :: Show b => (b -> a -> b) -> b -> [a] -> b
traceFoldl _ ini []     = ini
traceFoldl f ini (x:xs) = 
    let res = f ini x
     in traceShow res (traceFoldl f res xs)

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла = 0,25 * 10)
-- Подумайте, какую функцию сверки когда лучше использовать

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ ini []     = ini
myFoldr f ini (x:xs) = f x (myFoldr f ini xs)

or' :: [Bool] -> Bool
or' = myFoldr (||) False

length' :: [a] -> Int
length' xs = foldr (\_ acc -> acc + 1) 0 xs

-- По подсказке hlint можно дропнуть xs с обеих сторон и все равно все будет работать 
-- Т.е. последнюю строчку можно переписать как length' = foldr (\_ acc -> acc + 1) 0

maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
maximum' xs = Just $ foldr1 (\x acc -> if x > acc then x else acc) xs

-- hlint предлагает использовать max, но нам ведь так нельзя, да :с
-- foldr1, which is similar to foldr but doesn't require an initial accumulator value. 
-- Instead, it uses the first element of the list as the initial accumulator value
-- А еще hlint предложил добавить (Ord a), без этого не работало 
-- как я понял, это нужно было, чтобы показать компилятору, что мы работает с типом даннных, на котором можно определить <,>

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc ++ [x]) []

-- Аналогично примеру с length' дропнул xs с обеих сторон по указке hlint

filter' :: (a -> Bool) -> [a] -> [a]
filter' predicate = foldr (\x acc -> if predicate x then x : acc else acc) [] 

map' :: (a -> b) -> [a] -> [b]
map' f = myFoldr (\x acc -> f x : acc) []

head' :: [a] -> Maybe a
head' = myFoldr (\x _ -> Just x) Nothing

last' :: [a] -> Maybe a
last' [] = Nothing
last' (x:xs) = Just(L.foldl' (\_ lol -> lol) x xs) 

-- используйте L.foldl'
take' :: Int -> [a] -> [a]
take' n = L.foldl' (\acc x -> if length acc < n then acc ++ [x] else acc) []

-- используйте foldr
take'' :: Int -> [a] -> [a]
take'' n = myFoldr (\x acc -> if length acc < n then x : acc else acc) []

------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла = 1 + 1 + 0,5)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше
--   (1 балл)
--
insert :: (Ord a) => [a] -> a -> [a]
insert [] mem = [mem]
insert (x:xs) mem 
    | mem < x   = mem : x : xs
    | otherwise = x : insert xs mem

-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали
--
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert (insertionSort xs) x

------------------------------------------------------------------------------------------------

-- 4. Числа Фибоначчи (2,5 баллa = 0,5 + 1 + 1)

-- | аналогична zip, но применяет к элементам пары функцию (0,5 балла)
--
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
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
--     __индекс__ которой является числом Фибоначчи. (1 балл)
-- Пока не придумал, как это сделать 
fibSum :: [Int] -> Int
fibSum = undefined

------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла = 0,25 + 1 + 1)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков. (0,25 балла)
--
data Tree a = Node a [Tree a] -- 'YourTree' — просто заглушка. Вместо неё вам надо будет написать своё описание типа данных 'Tree'

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs tree = bfsHelper [tree]
  where
    bfsHelper [] = []  -- Базовый случай: обход завершен, список пустой
    bfsHelper nodes = map nodeValue nodes ++ bfsHelper nextLevelNodes
      where
        nodeValue (Node val _) = val
        nextLevelNodes = concatMap nodeChildren nodes
        nodeChildren (Node _ children) = children

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs (Node val children) = val : concatMap dfs children

-- извлекли значение в ноде и его потомков в (Node val children)
-- затем объединяем это значение с примененным с помощью concatMap dfs на потомков

------------------------------------------------------------------------------------------------
