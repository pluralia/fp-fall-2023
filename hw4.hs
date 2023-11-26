{- cabal:
build-depends: base, containers, text, vector
-}

import Data.Char (ord)
import Data.Foldable (foldl', foldr, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import qualified Data.IntMap as M

-- info
-- list is represented as a nodes with links -> : and el
-- String == [Char]
-- <> == ++
-- String ++ String
-- [Char] ++ [Char]
-- 'c' -> Char

-- length :: [a] -> Int
-- length [] = 0
-- length (x: xs) 1 + length xs

-- (!!) :: [a] -> Int -> a
-- (!!) [] _ = error
-- (!!) (a: _) 0 = a
-- (!!) (a: xs) idx = (!!) xs (idx - 1)

--  groupBy :: (a -> a - Bool) -> [a] -> [ [a] ]
-- non-empty

-- data NonEmpty a = a :| [a]
-- headNE :: NonEmpty a -> a
-- headNE (a :| _) = a

-- groupBy :: (a -> a -> Bool) -> [a] -> [NonEmpty a]

-- O(1) -> length, index
-- intercalate, map

-- compact storage
-- 2nd part :->

-- Vector <-
-- vec :: V.Vector Int <- parametrized
-- vec = [1, 2, 3, 4]

-- toList
-- fromList
-- O(1) length, index

-- T.pack :: String -> T.Text
-- T.unpack :: T.Text -> String


-- maps
-- import qualified Data.Map.Strict as M
-- M.fromList :: [(k, v)] -> M.Map k v
-- M.toList ::  M.Map k v -> [(k, v)]

-- M.! :: M.Map k v -> k -> v - unsafe
-- M.!? :: M.Map k v -> k -> Maybe v

-- M.member :: k -> M.Map k v -> Bool
-- M.insert :: k -> v -> M.Map k v -> M.Map k v
-- M.delete  :: k -> M.Map k v -> M.Map k v

-- T.foldl'
-- T.foldr
-- T -> [Char] but more effective
-- V.foldr
-- V.foldl'

-- V.ifoldl'
-- V.ifoldr

-- M.foldlWithKey' :: (a -> k -> b -> a) -> a -> M.Map k b -> a
-- M.foldrWithKey  :: (a -> k -> b -> a) -> a -> M.Map k b -> a

-- К каждой задаче приведите хотя бы 1 или 2 тестовых примера.
-- Подсказка: Text и Vector можно конкатенировать с помощью оператора '<>'.
--
-- Старайтесь чтобы код был читаемым, используете понятные имена переменных, блоки where и
-- красивое форматирование.
--
-- Условие "за один проход" означает, что сложность итого алгоритма должна быть не больше O(n).
--
-- Бонус: запишите решения в стиле point free там где это возможно и __не портит читаемость__
-- (до +1 балла к стандартным 10)
--
-- Многие из заданий напоминают то, что мы регулярно делаем в Biocad.

------------------------------------------------------------------------------------------------

-- 1. Дополнение нулями слева (0,5 балла)
--
-- Готовую функцию из пакета text использовать нельзя

padZero :: T.Text -> Int -> T.Text
padZero str width = undefined

------------------------------------------------------------------------------------------------

-- 3. Четные и нечетные (0,5 балла)
--
-- Разделить список на подсписки элементов на четных и нечетных позициях используя свертку. Порядок элементов
-- должен сохраняться.

evenodd :: [a] -> ([a], [a])
evenodd xs = undefined

------------------------------------------------------------------------------------------------

-- 4. Среднее (0,5 балла)
--
-- Посчитать среднее значение чисел в массиве с помощью свёртки за один проход

average :: V.Vector Double -> Double
average vec = undefined

------------------------------------------------------------------------------------------------

-- 5. GC состав (0,75 балла)
-- Нуклеотиды G и C физически чем-то отличаются от нуклеотидов A и T, поэтому их доля в последовательности — важная характеристика.
--
-- Посчитать долю G/C в последовательности с помощью свёртки за один проход.

gcContent :: T.Text -> Double
gcContent str = undefined

------------------------------------------------------------------------------------------------

-- 9. M.fromList (0,75 балл)
--
-- Реализовать `M.fromList :: [(k, v)] -> M.Map k v` с помощью свёрток `foldl'` и `foldr`. Объяснить
-- чем будет отличаться поведение этих вариантов.

fromListL :: (Ord k) => [(k, v)] -> M.Map k v
fromListL lst = undefined

fromListR :: (Ord k) => [(k, v)] -> M.Map k v
fromListR lst = undefined

------------------------------------------------------------------------------------------------

-- 10. Уникальные элементы (0,5 балла)
--
-- Оставить только уникальные элементы из списка. Порядок может измениться.
--
-- Для решения этой задачи можно использовать Map или тип Set из пакета containers:
-- https://hackage.haskell.org/package/containers-0.7/docs/Data-Set.html
--
-- Решение должно использовать свёртку по входному списку в один проход. Использовать fromList нельзя.

nubOrd :: (Ord a) => [a] -> [a]
nubOrd xs = undefined

------------------------------------------------------------------------------------------------

-- 11. Сложная: query parameters (1,25 балл)
--
-- В схеме URL могут использоваться query parameters: http://some.come/foo?a=1&b=2&c=hello
--
-- Соберите строку "a=1&b=2&c=hello" из `Map Text Text` используя `foldlWithKey'` или `foldrWithKey`.

buildQuery :: M.Map T.Text T.Text -> T.Text
buildQuery parameters = undefined

------------------------------------------------------------------------------------------------