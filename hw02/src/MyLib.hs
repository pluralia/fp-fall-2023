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
traceFoldl _ acc [] = acc
traceFoldl f acc (x:xs) = 
    let res = f (traceShow acc acc) x
    in traceFoldl f res xs

------------------------------------------------------------------------------------------------

-- 2. Реализуйте следующие функции с ипользованием свертки (2,5 балла = 0,25 * 10)
-- Подумайте, какую функцию сверки когда лучше использовать

-- используем правую свертку так как тогда функция закончит работу, как только встретит True
or' :: [Bool] -> Bool
or' = foldr (||) False

-- кажется, что нет разницы какую свертку мы используем
length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

-- я не придумала как можно эту функцию реализовать с левой сверткой, с правой интуитивно понятнее
maximum' :: [Int] -> Maybe Int
maximum' = foldr (maxMaybe . Just) Nothing
  where
    maxMaybe Nothing y = y
    maxMaybe x Nothing = x
    maxMaybe (Just x) (Just y) = Just (max x y)

maximum1' :: [Int] -> Maybe Int
maximum1' = foldl maxMaybe Nothing . map Just
  where
    maxMaybe Nothing y = y
    maxMaybe x Nothing = x
    maxMaybe (Just x) (Just y) = Just (max x y)

-- левая свертка логичнее всего подходит к этой функции, так как обрабатывает элементы слева направо
reverse' :: [a] -> [a]
--reverse' = foldl place_first []
--  where
--    place_first xs x = x : xs
reverse' = foldl (\xs x -> x : xs) []

-- какую свертку использовать неважно
filter' :: (a -> Bool) -> [a] -> [a]
--filter' p = foldr (choose p) []
-- where
--    choose p x xs = if p x then x : xs else xs
filter' p = foldr (\x xs -> if p x then x : xs else xs) []

-- какую свертку использовать неважно
map' :: (a -> b) -> [a] -> [b]
map' f = foldr helpMap []
  where
    helpMap x acc = f x : acc

-- правая свертка, так как мы получаем начало списка
head' :: [a] -> Maybe a
--head' = foldr helpHead Nothing
--  where
--    helpHead x y = Just x
head' = foldr (\x _ -> Just x) Nothing

-- левая свертка, так как нам нужен последний элемент списка
last' :: [a] -> Maybe a
--last' = foldl helpLast Nothing
--  where 
--    helpLast x y = Just y
last' = foldl (\_ y -> Just y) Nothing

-- используйте L.foldl'
take' :: Int -> [a] -> [a]
--take' n xs = snd (L.foldl' help_take (0, []) xs)
--  where
--    help_take (i, acc) x
--      | i < n     = (i + 1, acc ++ [x])
--      | otherwise = (i, acc)

take' n xs = snd $ L.foldl' help_take (0, []) (zip [1..] xs)
  where
    help_take (i, acc) (j, x)
      | j <= n    = (j, acc ++ [x])
      | otherwise = (i, acc)

-- используйте foldr
take'' :: Int -> [a] -> [a]
--take'' n xs = foldr help_take' (const []) xs 0
--  where
--    help_take' x f i
--      | i < n     = x : f (i + 1)
--      | otherwise = []
take'' n xs = foldr help_take' (const []) (zip [1..] xs) 0
  where
    help_take' :: (Int, a) -> (Int -> [a]) -> Int -> [a]
    help_take' (j, x) f i
      | j <= n    = x : f (i + 1)
      | otherwise = []
------------------------------------------------------------------------------------------------

-- 3. Сортировки (2,5 балла = 1 + 1 + 0,5)

-- | Быстрая сортировка (1 балл)
--
quicksort :: [Int] -> [Int]
quicksort = foldr partition []
  where
    partition x [] = [x]
    partition x xs = 
      let y = head xs
          ys = tail xs
      in if x <= y 
         then x : xs 
         else y : partition x ys

-- | Функция, которая вставляет в уже отсортированный список элементов
--   новый элемент на такую позицию, что все элементы левее будут меньше или
--   равны нового элемента, а все элементы справа будут строго больше
--   (1 балл)
--
insert :: [Int] -> Int -> [Int]
insert xs n = foldr insertInOrder [n] xs
  where
--    insertInOrder x acc
--      | x <= n && head acc == n = x : acc
--      | x <= n = x : acc
--      | head acc == n = n : x : tail acc
--      | otherwise = n : x : acc
      insertInOrder x [] = [x]
      insertInOrder x acc@(_ : ys)
        | x <= n = x : acc
        | otherwise = n : x : ys

-- | Сортировка вставками (0,5 балла)
--   В реализации можно использовать функцию insert, если вы её реализовали
--

insertionSort :: Ord a => [a] -> [a]
--insertionSort = foldr insert []
insertionSort [] = []
insertionSort (x:xs) = insert' x (insertionSort xs)
  where
    insert' x2 [] = [x2]
    insert' x2 acc@(y:ys)
      | x2 <= y = x2 : acc
      | otherwise = y : insert' x2 ys

------------------------------------------------------------------------------------------------

-- 4. Числа Фибоначчи (2,5 баллa = 0,5 + 1 + 1)

-- | аналогична zip, но применяет к элементам пары функцию (0,5 балла)
--
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = foldr helpMyZipWith [] (zip xs ys)
  where
    helpMyZipWith (a, b) acc = f a b : acc

myZipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith1 f = zipWith (curry helpMyZipWith)
  where
    helpMyZipWith (a, b) = f a b

myZipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith2 f = zipWith (\ a b -> f a b)

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
fibs =  0 : 1 : myZipWith (+) fibs (tail fibs)

-- | Считает сумму всех элементов списка, которые находятся на позиции, 
--     __индекс__ которой является числом Фибоначчи. (1 балл)
--
fibSum1 :: [Int] -> Int
fibSum1 xs = result
  where
    indexedElements = zip [0..] xs
    fibIndices = take (length xs) fibs
    result = foldl processElement 0 indexedElements
    processElement acc (i, x)
      | i `elem` fibIndices = acc + x
      | otherwise = acc

fibSum :: [Int] -> Int
fibSum xs = foldl processElement 0 . zip [0..] $ xs
  where
    processElement acc (i, x)
      | i `elem` fibIndices = acc + x
      | otherwise = acc
    fibIndices = take (length xs) fibs


------------------------------------------------------------------------------------------------

-- 6. Деревья (2,25 балла = 0,25 + 1 + 1)

-- | Реализуйте тип данных 'Tree' — дерева, у узлов которого может быть произвольное
--   число потомков. (0,25 балла)
--
data Tree a = Node a [Tree a] -- 'YourTree' — просто заглушка. Вместо неё вам надо будет написать своё описание типа данных 'Tree'

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в ширину (1 балл)
--
bfs :: Tree a -> [a]
bfs tree = help_bfs [tree]
  where
    help_bfs [] = []
    help_bfs xs = nodeValues ++ subTreeValues
      where
        nodeValues = map rootValue xs
        subTreesList = concatMap subTrees xs
        subTreeValues = help_bfs subTreesList
    rootValue (Node a _) = a
    subTrees (Node _ ts) = ts

-- | Принимает дерево, а возвращает список значений в его нодах в порядке обхода в глубину (1 балл)
--
dfs :: Tree a -> [a]
dfs tree = help_dfs tree []
  where
    help_dfs (Node value subTrees) acc = newValueAcc
      where
        newValueAcc = value : subTreesValues
        subTreesValues = foldr help_dfs acc subTrees


------------------------------------------------------------------------------------------------
