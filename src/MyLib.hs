
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

traceFoldl :: Show b => (b -> a -> b) -> b -> [a] -> b
traceFoldl _ acc []       = acc
traceFoldl f acc (x : xs) = 
    let res = f acc x
        in traceFoldl f (traceShow res res) xs

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла = 0,25 * 10)
-- Подумайте, какую функцию сверки когда лучше использовать

or' :: [Bool] -> Bool
or' = foldr (||) False
-- or' = foldr (\x b -> x || b) False

length' :: [a] -> Int
length' = L.foldl' (\acc _ -> acc + 1) 0

maximum' :: [Int] -> Maybe Int
maximum' = L.foldl' (\acc x -> if all (x >) acc then Just x else acc) Nothing
-- комментарии для меня: 
-- any, exists и all выступают как кванторы и работают на таких штуках, как MayBe, Either и прочие (fmap вроде тоже)
-- maximum' = L.foldl' (\b x -> if (b == Nothing || (any ((>) x) b)) then (Just x) else b) Nothing

reverse' :: [a] -> [a]
reverse' = L.foldl' (flip (:)) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

head' :: [a] -> Maybe a
head' = foldr (\x _ -> Just x) Nothing

last' :: [a] -> Maybe a
last' = L.foldl' (\_ x -> Just x) Nothing

-- используйте L.foldl'
take' :: Int -> [a] -> [a]
take' cnt = reverse . L.foldl' (\acc x -> if length' acc < cnt then x : acc else acc) []
-- асимптотика O(n^2) : потому что при каждой итерации считается длина

-- используйте foldr
take'' :: Int -> [a] -> [a]
take'' cnt xs = snd $ foldr takeHelper (length' xs - cnt, []) xs
    where
        takeHelper :: a -> (Int, [a]) -> (Int, [a])
        takeHelper x acc = 
            if fst acc <= 0 
                then (0, x : snd acc) 
                else (fst acc - 1, [])

------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла = 1 + 1 + 0,5)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (filter' (<= x) xs) ++ [x] ++ quicksort (filter' (> x) xs)

-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше
--   (1 балл)
--
insert :: Ord a => [a] -> a -> [a]
insert arr v = filter' (<= v) arr ++ [v] ++ filter' (> v) arr
-- Ord - любые сравниваемые объекты

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
myZipWith _ []     _      = []
myZipWith _ _      []     = []
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
fibs :: [Int]
fibs = 0 : fibs1
    where
        fibs1 :: [Int]
        fibs1 = 1 : myZipWith (+) fibs1 fibs


-- | Считает сумму всех элементов списка, которые находятся на позиции, 
--     __индекс__ которой является числом Фибоначчи. (1 балл)
--
fibSum :: [Int] -> Int
fibSum arr = foldr (\x acc -> if len <= x then 0 else (arr !! x) + acc) 0 fibs
    where len = length' arr

------------------------------------------------------------------------------------------------

-- 5. Деревья (2,25 балла = 0,25 + 1 + 1)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков. (0,25 балла)
--
data Tree a = Node 
              {  val :: a
              ,  child :: [Tree a]
              }

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs node = bfsHelper [node]
  where
    bfsHelper :: [Tree a] -> [a]
    bfsHelper [] = []
    bfsHelper c  = map (\(Node v _) -> v) c ++ bfsHelper (foldr (\(Node _ ch) acc -> ch ++ acc) [] c)

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs (Node v c) = v : foldr (\x acc -> dfs x ++ acc) [] c

------------------------------------------------------------------------------------------------